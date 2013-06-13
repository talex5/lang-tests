open Constants;;
open Support;;

let do_bindings sel (path : string option) (env : string list) =
  let process env child : string list = match Binding.parse_binding child with
  | None -> env
  | Some (Binding.EnvironmentBinding _ as b) -> Binding.do_env_binding b path env
  in
  let elem = Selections.get_elem sel in
  List.fold_left process env (elem.Qdom.child_nodes)
;;

let prepare_env stores selections (env : string list) =
  let get_path = function
    | Selections.CacheSelection _ as sel -> Some (Selections.get_path stores sel)
    | Selections.LocalSelection (path, _) -> Some path
    | Selections.PackageSelection _ -> None in
  let process_sel id sel env = (
    do_bindings sel (get_path sel) env
    (* do deps *)
    (* do commands *)
  ) in Selections.StringMap.fold process_sel selections.Selections.selections env
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
        let command_path = match command_sel with
          | Selections.CacheSelection _ -> Filename.concat (Selections.get_path stores command_sel) command_rel_path
          | Selections.LocalSelection (path, _) -> Filename.concat path command_rel_path
          | Selections.PackageSelection _ -> command_rel_path
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
  let original_env = Array.to_list (Unix.environment ()) in
  let env = prepare_env stores sels original_env in
  let prog_args = build_command stores sels env @ args in
  Unix.execve (List.hd prog_args) (Array.of_list prog_args) (Array.of_list env);;
