open Support;;
open Constants;;

type which_end = Prepend | Append;;
type add_mode = {pos :which_end; default :string option; separator :string};;

type mode =
  | Add of add_mode
  | Replace;;

type env_source =
  | InsertPath of filepath
  | Value of string;;

type exec_type = InPath | InVar;;
type env_binding = {var_name: varname; mode: mode; source: env_source};;
type exec_binding = {exec_type: exec_type; name: string; command: string};;

type binding =
| EnvironmentBinding of env_binding
| ExecutableBinding of exec_binding;;

let get_source b =
  let get name = ZI.get_attribute_opt name b in
  match (get "insert", get "value") with
  | (None, None) -> failwith "Missing 'insert' or 'value'"
  | (Some i, None) -> InsertPath i
  | (None, Some v) -> Value v
  | (Some _, Some _) -> failwith "Can't use 'insert' and 'value' together"
;;

let get_mode b =
  let get name = Qdom.get_attribute_opt ("", name) b in
  match default "prepend" (get "mode") with
  | "prepend" -> Add {pos = Prepend; default = get "default"; separator = default path_sep (get "separator")}
  | "append" -> Add {pos = Append; default = get "default"; separator = default path_sep (get "separator")}
  | "replace" -> Replace
  | x -> failwith("Unknown <environment> mode: " ^ x)
;;

let parse_binding elem =
  let get_opt name = Qdom.get_attribute_opt ("", name) elem in
  let get name = Qdom.get_attribute ("", name) elem in
  match ZI.tag elem with
  | Some "environment" -> Some (EnvironmentBinding {var_name = get "name"; mode = get_mode elem; source = get_source elem})
  | Some "executable-in-path" -> Some (ExecutableBinding {exec_type = InPath; name = get "name"; command = default "run" (get_opt "command")})
  | Some "executable-in-var" -> Some (ExecutableBinding {exec_type = InVar; name = get "name"; command = default "run" (get_opt "command")})
  | Some "overlay" | Some "binding" -> failwith "unsupporting binding type"
  | _ -> None
;;

let collect_bindings root =
  let bindings = ref [] in

  (* If node is a binding, add it to bindings. *)
  let process_binding iface node = match parse_binding node with
  | None -> ()
  | Some binding -> bindings := (iface, binding) :: !bindings in

  (* If node is a dependency, add its bindings. *)
  let process_dep node = match ZI.tag node with
  | Some "requires" | Some "runner" -> (ZI.iter (process_binding (ZI.get_attribute "interface" node)) node; true)
  | _ -> false in

  (* A command contains dependencies and bindings *)
  let process_command_child iface node =
    if process_dep node then ()
    else process_binding iface node in
  
  (* A selection contains commands, dependencies and bindings *)
  let process_sel_child iface node = match ZI.tag node with
  | Some "command" -> ZI.iter (process_command_child iface) node
  | _ ->
      if process_dep node then ()
      else process_binding iface node in

  let process_sel node =
    let iface = ZI.get_attribute "interface" node in
    ZI.iter (process_sel_child iface) node
  in
  
  ZI.iter_with_name process_sel root "selection";

  !bindings
;;

let get_default name = match name with
  | "PATH" -> Some "/bin:/usr/bin"
  | "XDG_CONFIG_DIRS" -> Some "/etc/xdg"
  | "XDG_DATA_DIRS" -> Some "/usr/local/share:/usr/share"
  | _ -> None
;;

let prepend name value sep env =
  let old_value = Env.find name env in
  Env.putenv name (value ^ sep ^ old_value) env
;;

let calc_new_value name mode value env =
  match mode with
  | Replace -> value
  | Add {pos; default; separator} ->
    let old_value = match Env.find_opt name env with
      | Some _ as v -> v                  (* current value of variable *)
      | None -> match default with
        | Some _ as d -> d                (* or the specified default *)
        | None -> get_default name        (* or the standard default *)
    in
    match old_value with
    | None -> value
    | Some old ->
      match pos with
      | Prepend -> value ^ separator ^ old
      | Append -> old ^ separator ^ value;;

let find_opt key map =
  try Some (StringMap.find key map)
  with Not_found -> None
;;

let do_env_binding env impls = function
| (iface, EnvironmentBinding {var_name; mode; source}) -> (
    let value = match source with
    | Value v -> Some v
    | InsertPath i ->
      match find_opt iface impls with
      | None -> None  (* optional, non-selected dependency *)
      | Some (_, None) -> None  (* a PackageSelection; skip binding *)
      | Some (_, Some p) -> Some (p +/ i)
    in
    match value with
    | None -> ()     (* Nothing to bind *)
    | Some v -> Env.putenv var_name (calc_new_value var_name mode v env) env
)
| _ -> ();
