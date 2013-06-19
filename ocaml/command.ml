open Support;;
open Constants;;

let get_command name elem =
  let is_command node = ((ZI.tag node = Some "command") && (ZI.get_attribute "name" node = name)) in
  try Qdom.find is_command elem
  with Not_found -> failwith ("No <command> with name '" ^ name ^ "'")
;;

let re_template = Str.regexp ("\\$\\(\\$\\|\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\|{[^}]*}\\)")

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
    let item_from = ZI.get_attribute "item-from" node in
    let separator = default path_sep (ZI.get_attribute_opt "separator" node) in
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

let get_runner elem =
  match ZI.map (fun a -> a) elem "runner" with
    | [] -> None
    | [runner] -> Some runner
    | _ -> failwith "Multiple runners!"
;;

(* Build up the argv array to execute this command *)
let rec build_command impls command_iface command_name env : string list =
  let (command_sel, command_impl_path) = StringMap.find command_iface impls in
  let command = get_command command_name command_sel in
  let command_rel_path = ZI.get_attribute_opt "path" command in

  (* args for the first command *)
  let command_args = get_args command env in
  let args = (match command_rel_path with
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
  match get_runner command with
  | None -> (
      if command_rel_path = None then
        failwith ("Missing path on <command> with no <runner> in " ^ command_iface)
      else
        args
    )
  | Some runner ->
      let runner_args = get_args runner env in
      let runner_command_name = default "run" (ZI.get_attribute_opt "command" runner) in
      (build_command impls (ZI.get_attribute "interface" runner) runner_command_name env) @ runner_args @ args
;;
