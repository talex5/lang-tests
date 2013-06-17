open Constants;;
open Support;;

type exec_binding = {
  iface_uri: string;
  exec_type: Binding.exec_type;
  name: string;
  command: string;
} ;;

type env = {
  vars: Env.env;
  exec_bindings: exec_binding list;
};;

let re_exec_name = Str.regexp "^[^./'][^/']*$";;

let validate_exec_name name =
  if Str.string_match re_exec_name name 0 then
    ()
  else
    failwith ("Invalid name in executable binding: " ^ name)

let do_bindings elem (path : string option) iface (env : env) : env =
  let process env child : env =
    match Binding.parse_binding child with
    | None -> env
    | Some (Binding.ExecutableBinding (exec_type, name, command)) ->
        let new_binding = {iface_uri = iface; exec_type; name; command} in
        {env with exec_bindings = new_binding :: env.exec_bindings}
    | Some (Binding.EnvironmentBinding _ as b) ->
        {env with vars = Binding.do_env_binding b path env.vars}
    in
  List.fold_left process env (elem.Qdom.child_nodes)
;;

type importance = Essential | Recommended;;

let importance dep = match Qdom.get_attribute_opt ("", "importance") dep with
  | Some "essential" | None -> Essential
  | Some "recommended" -> Recommended
  | Some _ -> Recommended
;;

let ensure_runenv config =
  let main_dir = Basedir.save_cache_path ("0install.net" +/ "injector") config.Config.basedirs in
  let runenv = main_dir +/ "runenv.native" in
  if Sys.file_exists runenv then
    ()
  else
    Unix.symlink (config.Config.resource_dir +/ "runenv.native") runenv
;;

let re_id = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
let re_template = Str.regexp ("\\$\\(\\$\\|" ^ re_id ^ "\\|{[^}]*}\\)")

(* Perform $ substitutions on [template], taking values from [env] *)
let expand_arg template env =
  let remove_braces s =
    let l = String.length s in
    if s.[0] = '{' then (
      assert (s.[l - 1] = '}');
      String.sub s 1 (l - 2)
    ) else s; in
  let expand s = match (Str.matched_group 1 s) with
  | "$" -> "$"
  | "" | "{}" -> failwith ("Error: empty variable name in template: " ^ template)
  | m -> Env.find (remove_braces m) env in
  Str.global_substitute re_template expand template
;;

(* Return a list of string arguments by expanding <arg> and <for-each> children of [elem] *)
let rec get_args elem env =
  let process args child = match ZI.tag child with
  | Some "arg" -> (expand_arg child.Qdom.last_text_inside env) :: args
  | Some "for-each" -> (expand_foreach child env) @ args
  | _ -> args in
  List.fold_left process [] (elem.Qdom.child_nodes)
and expand_foreach node env =
  let item_from = Qdom.get_attribute ("", "item-from") node in
  let separator = default path_sep (Qdom.get_attribute_opt ("", "separator") node) in
  match Env.find_opt item_from env with
  | None -> []
  | Some source ->
      let rec loop = function
        | [] -> []
        | x::xs ->
            let new_args = get_args node (Env.putenv "item" x env) in
            new_args @ (loop xs) in
      loop (Str.split_delim (Str.regexp_string separator) source)
;;

(* Build up the argv array to execute this command *)
let rec build_command stores selections command_iface command_name env : string list =
  let command_sel = Selections.get command_iface selections in
  let command = Selections.get_command command_name command_sel in

  (* args for the first command *)
  let command_args = get_args (Command.get_elem command) env in
  let args = (match Command.get_path command with
    | None -> command_args
    | Some command_rel_path ->
        let command_path =
          match Selections.get_path stores command_sel with
          | None -> (   (* PackageSelection *)
            if (Filename.is_relative  command_rel_path) then
              failwith ("Relative path in package - TODO")
            else
              command_rel_path      
          )
          | Some dir -> (
            if (Filename.is_relative command_rel_path) then
              Filename.concat dir command_rel_path
            else
              failwith ("Absolute path in <command>: " ^ command_rel_path)
          )
        in
          command_path :: command_args
  ) in

  (* recursively process our runner, if any *)
  match Command.get_runner command with
  | None -> (
      if (Command.get_path command) = None then
        failwith ("Missing path on <command> with no <runner> in " ^ command_iface)
      else
        args
    )
  | Some runner ->
      let runner_args = get_args (Runner.get_elem runner) env in
      (build_command stores selections (Runner.get_interface runner) (Runner.get_command_name runner) env) @ runner_args @ args
;;

let prepare_env config selections vars : Env.env =
  let env = {vars; exec_bindings = []} in
  let lazy_ensure_runenv = lazy ( ensure_runenv config) in

  let do_dep (env : env) dep : env = (
    let dep_iface = Qdom.get_attribute ("", "interface") dep in
    let dep_sel_opt = match importance dep with
      | Essential -> Some (Selections.get dep_iface selections)
      | Recommended -> Selections.get_opt dep_iface selections
    in
    match dep_sel_opt with
    | None -> env
    | Some dep_sel -> do_bindings dep (Selections.get_path config.Config.stores dep_sel) dep_iface env
  ) in

  let do_deps elem (env : env): env = List.fold_left do_dep env (Selections.get_deps elem) in

  let process_sel id sel env = (
    let elem = Selections.get_elem sel in
    let sel_iface = Qdom.get_attribute ("", "interface") elem in
    let process_command env c = (
      let elem = (Command.get_elem c) in
      let env = do_bindings elem (Selections.get_path config.Config.stores sel) sel_iface env in
      do_deps elem env
    ) in
    let env = do_bindings elem (Selections.get_path config.Config.stores sel) sel_iface env in
    let env = do_deps elem env in
    List.fold_left process_command env (Selections.get_commands sel)
  ) in
  
  let {vars; exec_bindings} = Selections.StringMap.fold process_sel selections.Selections.selections env in

  let do_exec_binding vars {iface_uri; exec_type; name; command} = (
    validate_exec_name name;

    Lazy.force lazy_ensure_runenv;

    (* set up launcher symlink *)
    let exec_dir = Basedir.save_cache_path ("0install.net" +/ "injector" +/ "executables" +/ name) config.Config.basedirs in
    let exec_path = exec_dir ^ Filename.dir_sep ^ name in   (* TODO: windows *)

    if not (Sys.file_exists exec_path) then (
      (* TODO: windows *)
      Unix.symlink "../../runenv.native" exec_path;
      Unix.chmod exec_dir 0o500
    ) else ();

    let command_argv = build_command config.Config.stores selections iface_uri command env.vars in

    let env = match exec_type with
    | Binding.InPath -> Binding.prepend "PATH" exec_dir path_sep vars
    | Binding.InVar -> Env.putenv name exec_path vars in

    let open Yojson.Basic in
    let json :json = `List (List.map (fun a -> `String a) command_argv) in
    Env.putenv ("0install-runenv-" ^ name) (to_string json) env
  ) in

  (* Do delayed executable bindings, now that all environment variables have been set *)
  List.fold_left do_exec_binding vars exec_bindings
;;

let execute_selections (sels:Selections.selections) (args:string list) config =
  let original_env = Env.copy_current_env () in
  let env = prepare_env config sels original_env in
  let command = expect (sels.Selections.command) "No command specified" in
  let prog_args = (build_command config.Config.stores sels sels.Selections.interface command env) @ args in
  flush stdout;
  flush stderr;
  Unix.execve (List.hd prog_args) (Array.of_list prog_args) (Env.to_array env);;
