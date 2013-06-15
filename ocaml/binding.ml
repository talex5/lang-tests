open Support;;

type which_end = Prepend | Append;;
type add_mode = {pos :which_end; default :string option; separator :string};;

type mode =
  | Add of add_mode
  | Replace;;

type env_source =
  | InsertPath of string
  | Value of string
;;

type exec_type = InPath | InVar;;

type binding =
| EnvironmentBinding of (string * mode * env_source)
| ExecutableBinding of (exec_type * string * string);;  (* name, command *)

let get_source b =
  let get name = Qdom.get_attribute_opt ("", name) b in
  match (get "insert", get "value") with
  | (None, None) -> failwith "Missing 'insert' or 'value'"
  | (Some i, None) -> InsertPath i
  | (None, Some v) -> Value v
  | (Some i, Some v) -> failwith "Can't use 'insert' and 'value' together"
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
  match elem.Qdom.tag with
  | (xmlns_feed, "environment") -> Some (EnvironmentBinding (get "name", get_mode elem, get_source elem))
  | (xmlns_feed, "executable-in-path") -> Some (ExecutableBinding (InPath, get "name", default "run" (get_opt "command")))
  | (xmlns_feed, "executable-in-var") -> Some (ExecutableBinding (InVar, get "name", default "run" (get_opt "command")))
  | (xmlns_feed, "overlay") | (xmlns_feed, "binding") -> failwith "unsupporting binding type"
  | _ -> None
;;

(*
let do_binding b env = match source with
| InsertPath path -> failwith "insert not supported"
| Value value -> update_env mode value env
;;
*)

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
    (* Find the value to join it with *)
    let old_value = (match Env.find_opt name env with
      | Some _ as v -> v                  (* current value of variable *)
      | None -> (match default with
        | Some _ as d -> d                (* or the specified default *)
        | None -> get_default name        (* or the standard default *)
      )
    ) in
    match old_value with
    | None -> value
    | Some old ->
      match pos with
      | Prepend -> value ^ separator ^ old
      | Append -> old ^ separator ^ value
;;

let prepend name value sep env =
  let old_value = Env.find name env in
  Env.putenv name (value ^ sep ^ old_value) env
;;

let do_env_binding b path env = match b with
| EnvironmentBinding (name, mode, source) -> (
    let value = match source with
    | InsertPath i -> (
      match path with
      | None -> None  (* a PackageSelection; skip binding *)
      | Some p -> Some (Filename.concat p i)
    )
    | Value v -> Some v
    in
    match value with
    | None -> env     (* Nothing to bind *)
    | Some v -> Env.putenv name (calc_new_value name mode v env) env
)
| _ -> failwith "Not an environment binding"
;;
