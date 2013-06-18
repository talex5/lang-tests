open Constants;;
open Support;;

let re_exec_name = Str.regexp "^[^./'][^/']*$";;

let validate_exec_name name =
  if Str.string_match re_exec_name name 0 then
    ()
  else
    failwith ("Invalid name in executable binding: " ^ name)

let ensure_runenv config =
  let main_dir = Basedir.save_path ("0install.net" +/ "injector") config.Config.basedirs.Basedir.cache in
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
let get_args elem env =
  let rec get_args_loop elem =
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
              let () = Env.putenv "item" x env in   (* undo *)
              let new_args = get_args_loop node in
              new_args @ (loop xs) in
        loop (Str.split_delim (Str.regexp_string separator) source)
  in get_args_loop elem
;;

(* Build up the argv array to execute this command *)
let rec build_command impls command_iface command_name env : string list =
  let (command_sel, command_impl_path) = StringMap.find command_iface impls in
  let command = Selections.get_command command_name command_sel in

  (* args for the first command *)
  let command_args = get_args (Command.get_elem command) env in
  let args = (match Command.get_path command with
    | None -> command_args
    | Some command_rel_path ->
        let command_path =
          match command_impl_path with
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
      (build_command impls (Runner.get_interface runner) (Runner.get_command_name runner) env) @ runner_args @ args
;;

let do_exec_binding config env impls = function
  | (iface_uri, Binding.ExecutableBinding {Binding.exec_type; Binding.name; Binding.command}) -> (
    validate_exec_name name;

    (* set up launcher symlink *)
    let exec_dir = Basedir.save_path ("0install.net" +/ "injector" +/ "executables" +/ name) config.Config.basedirs.Basedir.cache in
    let exec_path = exec_dir ^ Filename.dir_sep ^ name in   (* TODO: windows *)

    if not (Sys.file_exists exec_path) then (
      (* TODO: windows *)
      Unix.symlink "../../runenv.native" exec_path;
      Unix.chmod exec_dir 0o500
    ) else ();

    let command_argv = build_command impls iface_uri command env in

    let () = match exec_type with
    | Binding.InPath -> Binding.prepend "PATH" exec_dir path_sep env
    | Binding.InVar -> Env.putenv name exec_path env in

    let open Yojson.Basic in
    let json :json = `List (List.map (fun a -> `String a) command_argv) in

    Env.putenv ("0install-runenv-" ^ name) (to_string json) env
  )
  | _ -> ()
;;

(* Make a map from InterfaceURIs to the selected <selection> and (for non-native packages) paths *)
let make_selection_map stores sels =
  let add_selection m sel =
    let path = Selections.get_path stores sel in
    let value = (sel, path) in
    StringMap.add (ZI.get_attribute "interface" sel) value m
  in ZI.fold_left add_selection StringMap.empty sels "selection"
;;

let execute_selections sels (args:string list) config =
  let env = Env.copy_current_env () in
  let impls = make_selection_map config.Config.stores sels in
  let bindings = Binding.collect_bindings impls sels in

  ensure_runenv config;

  (* Do <environment> bindings *)
  List.iter (Binding.do_env_binding env impls) bindings;

  (* Do <executable-in-*> bindings *)
  List.iter (do_exec_binding config env impls) bindings;

  let command = ZI.get_attribute "command" sels in
  let prog_args = (build_command impls (ZI.get_attribute "interface" sels) command env) @ args in
  flush stdout;
  flush stderr;
  Unix.execve (List.hd prog_args) (Array.of_list prog_args) (Env.to_array env);;
