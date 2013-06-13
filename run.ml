open Constants;;
open Support;;

type exec_binding = (string * Binding.exec_type * string * string);;  (* iface_uri, exec_type, name, command *)

type env = (string list * exec_binding list);;

let re_exec_name = Str.regexp "^[^./'][^/']*$";;

let validate_exec_name name =
  if Str.string_match re_exec_name name 0 then
    ()
  else
    failwith ("Invalid name in executable binding: " ^ name)

let do_bindings elem (path : string option) (env : env) : env =
  let process env child : env =
    let (vars, bindings) = env in
    match Binding.parse_binding child with
    | None -> env
    | Some (Binding.ExecutableBinding (exec_type, name, command)) -> (vars, ("URI", exec_type, name, command) :: bindings)
    | Some (Binding.EnvironmentBinding _ as b) -> (Binding.do_env_binding b path vars, bindings)
    in
  List.fold_left process env (elem.Qdom.child_nodes)
;;

let prepare_env stores selections env : string list =
  let do_dep (env : env) dep : env = (
    let dep_iface = Qdom.get_attribute ("", "interface") dep in
    let dep_sel = Selections.get dep_iface selections in   (* todo: Recommended *)
    do_bindings dep (Selections.get_path stores dep_sel) env
  ) in

  let do_deps elem (env : env): env = List.fold_left do_dep env (Selections.get_deps elem) in

  let process_sel id sel env = (
    let process_command env c = (
      let elem = (Command.get_elem c) in
      let env = do_bindings elem (Selections.get_path stores sel) env in
      do_deps elem env
    ) in
    let elem = Selections.get_elem sel in
    let env = do_bindings elem (Selections.get_path stores sel) env in
    let env = do_deps elem env in
    List.fold_left process_command env (Selections.get_commands sel)
  ) in
  
  let (vars, exec_bindings) = Selections.StringMap.fold process_sel selections.Selections.selections env in

  let do_exec_binding vars (iface_uri, exec_type, name, command) = (
    validate_exec_name name;

    (* todo: setup symlinks *)
    let exec_dir = "/home/tal/.cache/0install.net/injector/executables/" ^ name in
    let exec_path = exec_dir ^ Filename.dir_sep ^ name in

    match exec_type with
    | Binding.InPath -> Binding.prepend "PATH" exec_dir path_sep vars
    | Binding.InVar -> Binding.putenv name exec_path vars
  ) in

  (* Do delayed executable bindings, now that all environment variables have been set *)
  List.fold_left do_exec_binding vars exec_bindings
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
  | m -> getenv (remove_braces m) env in
  Str.global_substitute re_template expand template
;;

(* Return a list of string arguments by expanding <arg> and <for-each> children of [elem] *)
let rec get_args elem env =
  let process args child = match child.Qdom.tag with
  | (xmlns_feed, "arg") -> (expand_arg child.Qdom.last_text_inside env) :: args
  | (xmlns_feed, "for-each") -> (expand_foreach child env) @ args
  | _ -> args in
  List.fold_left process [] (elem.Qdom.child_nodes)
and expand_foreach node env =
  let item_from = Qdom.get_attribute ("", "item-from") node in
  let separator = default path_sep (Qdom.get_attribute_opt ("", "separator") node) in
  match getenv_opt item_from env with
  | None -> []
  | Some source ->
      let rec loop = function
        | [] -> []
        | x::xs ->
            let binding = "item=" ^ x in
            let new_args = get_args node (binding :: env) in
            new_args @ (loop xs) in
      loop (Str.split_delim (Str.regexp_string separator) source)
;;

(* Build up the argv array to execute this command *)
let build_command stores selections env =
  let rec do_command command_iface command_name : string list = (
    let command_sel = Selections.get command_iface selections in
    let command = Selections.get_command command_name command_sel in

    (* args for the first command *)
    let command_args = get_args (Command.get_elem command) env in
    let args = (match Command.get_path command with
      | None -> command_args
      | Some command_rel_path ->
          let command_path =
            match Selections.get_path stores command_sel with
            | None -> command_rel_path      (* PackageSelection *)
            | Some dir -> Filename.concat dir command_rel_path
          in
            command_path :: command_args
    ) in

    (* recursively process our runner, if any *)
    match Command.get_runner command with
    | None -> args
    | Some runner ->
        let runner_args = get_args (Runner.get_elem runner) env in
        (do_command (Runner.get_interface runner) (Runner.get_command_name runner)) @ runner_args @ args
  ) in do_command selections.Selections.interface (expect (selections.Selections.command) "No command specified")
;;

let execute_selections (sels:Selections.selections) (args:string list) stores =
  let original_env = (Array.to_list (Unix.environment ()), []) in
  let env = prepare_env stores sels original_env in
  let prog_args = build_command stores sels env @ args in
  flush stdout;
  flush stderr;
  Unix.execve (List.hd prog_args) (Array.of_list prog_args) (Array.of_list env);;
