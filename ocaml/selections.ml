open Constants;;

type impl_source =
  | CacheSelection of Stores.digest list
  | LocalSelection of string
  | PackageSelection
;;

exception InvalidSelections of string;;

let re_initial_slash = Str.regexp "^/";;
let re_package = Str.regexp "^package:";;

let get_digests elem =
  (* todo: ID *)
  let check_attr init ((ns, name), value) = match ns with
    | "" -> (name, value) :: init
    | _ -> init in
  let extract_digests init elem =
    List.fold_left check_attr init elem.Qdom.attrs in
  ZI.fold_left extract_digests [] elem "manifest-digest";;

let make_selection elem =
  let source = (match Qdom.get_attribute_opt ("", "local-path") elem with
  | Some path -> LocalSelection path
  | None -> let id = Qdom.get_attribute ("", "id") elem in
    if Str.string_match re_initial_slash id 0 then
      LocalSelection id   (* Backwards compatibility *)
    else if Str.string_match re_package id 0 then
      PackageSelection
    else
      CacheSelection (match get_digests elem with
      | [] ->
        let id = Qdom.get_attribute ("", "id") elem in
        raise (InvalidSelections ("Implementation '" ^ id ^ "' has no digests"))
      | digests -> digests
      )
  ) in source
;;

let get_path stores elem =
  match make_selection elem with
  | PackageSelection -> None
  | LocalSelection path -> Some path
  | CacheSelection digests -> Some (Stores.lookup_any digests stores)
;;

let get_command name elem =
  let is_command node = ((ZI.tag node = Some "command") && (ZI.get_attribute "name" node = name)) in
  let command_elem =
    try Qdom.find is_command elem
    with Not_found -> failwith ("No <command> with name '" ^ name ^ "'")
  in
  Command.make command_elem;;
