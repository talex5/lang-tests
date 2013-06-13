module StringMap = Map.Make(String);;
open Constants;;

type selection =
  | CacheSelection of (Qdom.element)
  | LocalSelection of (string * Qdom.element)
  | PackageSelection of (Qdom.element)
;;

type selections = {
        interface:string;
        command: string option;
        selections: selection StringMap.t;
};;

let re_initial_slash = Str.regexp "^/";;
let re_package = Str.regexp "^package:";;

let make_selection elem =
  match Qdom.get_attribute_opt ("", "local-path") elem with
  | Some path -> LocalSelection (path, elem)
  | None -> let id = Qdom.get_attribute ("", "id") elem in
    if Str.string_match re_initial_slash id 0 then
      LocalSelection (id, elem)   (* Backwards compatibility *)
    else if Str.string_match re_package id 0 then
      PackageSelection elem
    else
      CacheSelection elem;;

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

let get_digests elem =
  let check_attr init ((ns, name), value) = match ns with
    | "" -> (name, value) :: init
    | _ -> init in
  let extract_digests init elem =
    List.fold_left check_attr init elem.Qdom.attrs in
  Qdom.fold_left extract_digests [] elem (xmlns_feed, "manifest-digest");;

let get_elem = function
  | CacheSelection elem -> elem
  | LocalSelection (path, elem) -> elem
  | PackageSelection elem -> elem;;

let get_path stores sel = Stores.lookup_any (get_digests (get_elem sel)) stores;;

let get_command name sel =
  let elem = get_elem sel in
  let is_command node = ((node.Qdom.tag = (xmlns_feed, "command")) && (Qdom.get_attribute ("", "name") node = name)) in
  let command_elem = Qdom.find is_command elem in
  Command.make command_elem;;
