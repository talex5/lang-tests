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
  let get name = ZI.get_attribute_opt name b in
  match default "prepend" (get "mode") with
  | "prepend" -> Add {pos = Prepend; default = get "default"; separator = default path_sep (get "separator")}
  | "append" -> Add {pos = Append; default = get "default"; separator = default path_sep (get "separator")}
  | "replace" -> Replace
  | x -> failwith("Unknown <environment> mode: " ^ x)
;;

let parse_binding elem =
  let get_opt name = ZI.get_attribute_opt name elem in
  let get name = ZI.get_attribute name elem in
  match ZI.tag elem with
  | Some "environment" -> Some (EnvironmentBinding {var_name = get "name"; mode = get_mode elem; source = get_source elem})
  | Some "executable-in-path" -> Some (ExecutableBinding {exec_type = InPath; name = get "name"; command = default "run" (get_opt "command")})
  | Some "executable-in-var" -> Some (ExecutableBinding {exec_type = InVar; name = get "name"; command = default "run" (get_opt "command")})
  | Some "overlay" | Some "binding" -> failwith "unsupporting binding type"
  | _ -> None
;;

(* Return all bindings in document order *)
let collect_bindings impls root =
  let bindings = ref [] in

  (* If node is a binding, add it to bindings. *)
  let process_binding iface node = match parse_binding node with
  | None -> ()
  | Some binding -> bindings := (iface, binding) :: !bindings in

  (* If node is a dependency, add its bindings. *)
  let process_dep node = match ZI.tag node with
  | Some "requires" | Some "runner" ->
      let dep_iface = ZI.get_attribute "interface" node in
      if StringMap.mem dep_iface impls then (ZI.iter (process_binding dep_iface) node; true)
      else true
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

  List.rev !bindings
;;

let get_default name = match name with
  | "PATH" -> Some "/bin:/usr/bin"
  | "XDG_CONFIG_DIRS" -> Some "/etc/xdg"
  | "XDG_DATA_DIRS" -> Some "/usr/local/share:/usr/share"
  | _ -> None
;;

let calc_new_value name mode value env =
  match mode with
  | Replace -> value
  | Add {pos; default; separator} ->
    let add_to old = match pos with
      | Prepend -> value ^ separator ^ old
      | Append -> old ^ separator ^ value in
    match Env.find_opt name env with
      | Some v -> add_to v                  (* add to current value of variable *)
      | None -> match default with
        | Some d -> add_to d                (* or to the specified default *)
        | None -> match get_default name with    
          | Some d -> add_to d              (* or to the standard default *)
          | None -> value                   (* no old value; use new value directly *)
;;

let do_env_binding env impls = function
| (iface, EnvironmentBinding {var_name; mode; source}) -> (
    let add value = Env.putenv var_name (calc_new_value var_name mode value env) env in
    match source with
    | Value v -> add v
    | InsertPath i -> match StringMap.find iface impls with
      | (_, None) -> ()  (* a PackageSelection; skip binding *)
      | (_, Some p) -> add (p +/ i)
)
| _ -> ()
;;

let prepend name value separator env =
  let mode = Add {pos = Prepend; default = None; separator} in
  Env.putenv name (calc_new_value name mode value env) env
;;
