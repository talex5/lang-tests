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
  let m = Qdom.fold_left add StringMap.empty root (xmlns_feed, "selection") in
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
  Qdom.fold_left extract_digests [] elem (xmlns_feed, "manifest-digest");;

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
  Qdom.map Command.make elem (xmlns_feed, "command");;

let get_command name sel =
  let elem = get_elem sel in
  let is_command node = ((node.Qdom.tag = (xmlns_feed, "command")) && (Qdom.get_attribute ("", "name") node = name)) in
  let command_elem = Qdom.find is_command elem in
  Command.make command_elem;;

let get_deps elem =
  let is_dep elem = match elem.Qdom.tag with
  | (xmlns_feed, "requires") | (xmlns_feed, "runner") -> true
  | _ -> false
  in List.filter is_dep elem.Qdom.child_nodes
;;
