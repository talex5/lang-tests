open Support;;

type which_end = Prepend | Append;;

type mode =
  | Add of which_end * string option * string (* default, separator *)
  | Replace;;

type source =
  | InsertPath of string
  | Value of string
;;

type binding = EnvironmentBinding of (string * mode * source);;

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
  | "prepend" -> Add (Prepend, get "default", default path_sep (get "separator"))
  | "append" -> Add (Append, get "default", default path_sep (get "separator"))
  | "replace" -> Replace
  | x -> failwith("Unknown <environment> mode: " ^ x)
;;

let parse_binding elem =
  let get name = Qdom.get_attribute ("", name) elem in
  match elem.Qdom.tag with
  | (xmlns_feed, "environment") -> Some (EnvironmentBinding (get "name", get_mode elem, get_source elem))
  | (xmlns_feed, "executable-in-path") | (xmlns_feed, "executable-in-var") | (xmlns_feed, "overlay") | (xmlns_feed, "binding") ->
      failwith "unsupporting binding type"
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
  | Add (which_end, default, sep) ->
    (* Find the value to join it with *)
    let old_value = (match getenv_opt name env with
      | Some _ as v -> v                  (* current value of variable *)
      | None -> (match default with
        | Some _ as d -> d                (* or the specified default *)
        | None -> get_default name        (* or the standard default *)
      )
    ) in
    match old_value with
    | None -> value
    | Some old ->
      match which_end with
      | Prepend -> value ^ sep ^ old
      | Append -> old ^ sep ^ value
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
    | Some v ->
        let new_value = calc_new_value name mode v env in
        Printf.printf "%s=%s" name new_value;
        (name ^ "=" ^ new_value) :: env
)
;;
