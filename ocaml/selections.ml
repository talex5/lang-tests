module StringMap = Map.Make(String);;
open Constants;;

type impl_source =
  | CacheSelection
  | LocalSelection of string
  | PackageSelection
;;

type selection = (impl_source * Qdom.element);;

type selections = {
        interface:string;
        command: string option;
        selections: selection StringMap.t;
};;

exception InvalidSelections of string;;

let re_initial_slash = Str.regexp "^/";;
let re_package = Str.regexp "^package:";;

let make_selection elem =
  let source = (match Qdom.get_attribute_opt ("", "local-path") elem with
  | Some path -> LocalSelection path
  | None -> let id = Qdom.get_attribute ("", "id") elem in
    if Str.string_match re_initial_slash id 0 then
      LocalSelection id   (* Backwards compatibility *)
    else if Str.string_match re_package id 0 then
      PackageSelection
    else
      CacheSelection
  ) in (source, elem)
;;

let rec find_selections root =
  let add m node = (
    let id = Qdom.get_attribute ("", "interface") node in
    assert (not (StringMap.mem id m));
    StringMap.add id (make_selection node) m
  ) in
  let m = ZI.fold_left add StringMap.empty root "selection" in
  m

let make root = {
  interface = Qdom.get_attribute ("", "interface") root;
  command = Some "run";
  selections = find_selections root
};;

let get interface sels =
  try StringMap.find interface sels.selections
  with Not_found -> failwith ("Interface '" ^ interface ^ "' not in selections")
;;

let get_opt interface sels =
  try Some (StringMap.find interface sels.selections)
  with Not_found -> None
;;

let get_digests elem =
  (* todo: ID *)
  let check_attr init ((ns, name), value) = match ns with
    | "" -> (name, value) :: init
    | _ -> init in
  let extract_digests init elem =
    List.fold_left check_attr init elem.Qdom.attrs in
  ZI.fold_left extract_digests [] elem "manifest-digest";;

let get_elem (impl_source, elem) = elem;;

let get_path stores (impl_source, elem) =
  match impl_source with
  | PackageSelection  -> None
  | LocalSelection path -> Some path
  | CacheSelection ->
    match get_digests elem with
    | [] ->
      let id = Qdom.get_attribute ("", "id") elem in
      raise (InvalidSelections ("Implementation '" ^ id ^ "' has no digests"))
    | digests -> Some (Stores.lookup_any digests stores);;

let get_commands (impl_source, elem) =
  ZI.map Command.make elem "command";;

let get_command name sel =
  let elem = get_elem sel in
  let is_command node = ((ZI.tag node = Some "command") && (ZI.get_attribute "name" node = name)) in
  let command_elem =
    try Qdom.find is_command elem
    with Not_found -> failwith ("No <command> with name '" ^ name ^ "'")
  in
  Command.make command_elem;;

let get_deps elem =
  let is_dep elem = match ZI.tag elem with
  | Some "requires" | Some "runner" -> true
  | _ -> false
  in List.filter is_dep elem.Qdom.child_nodes
;;
