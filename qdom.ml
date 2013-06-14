open Support;;

type element = {
  tag: Xmlm.name;
  mutable attrs: Xmlm.attribute list;
  mutable child_nodes: element list;
  mutable text_before: string;        (** The text node immediately before us *)
  mutable last_text_inside: string;   (** The last text node inside us with no following element *)
};;

exception InvalidXML of string;;

(* Parse all elements from here to the next close tag and return those elements *)
let rec parse_nodes i prev_siblings prev_text =
  if Xmlm.eoi i then
    (prev_siblings, prev_text)
  else
    match Xmlm.input i with
      | `Data s -> parse_nodes i prev_siblings (prev_text ^ s)
      | `Dtd dtd -> parse_nodes i prev_siblings prev_text
      | `El_end -> (prev_siblings, prev_text)
      | `El_start (tag, attrs) -> (
        let child_nodes, trailing_text = parse_nodes i [] "" in
        let new_node = {
          tag = tag;
          attrs = attrs;
          child_nodes = child_nodes;
          text_before = prev_text;
          last_text_inside = trailing_text;
        } in parse_nodes i (new_node :: prev_siblings) ""
      );;

let parse_input i = try (
  match parse_nodes i [] "" with
  | [root], "" -> root
  | _ -> failwith("Expected single root node in XML")
) with Xmlm.Error ((line, col), err) ->
  raise (InvalidXML (Printf.sprintf "[%d:%d] %s" line col (Xmlm.error_message err)))
;;

let parse_file path =
  try with_open path (fun ch -> parse_input (Xmlm.make_input (`Channel ch)))
  with InvalidXML msg -> raise (InvalidXML(msg ^ " in " ^ path))

let get_attribute attr elem = try
    List.assoc attr elem.attrs
  with
    Not_found ->
      let (ans, aname) = attr in
      let (ens, ename) = elem.tag in
      failwith ("Missing attribute {" ^ ans ^ "}" ^ aname ^ " on {" ^ ens ^ "}" ^ ename);;

let get_attribute_opt attr elem = try
    Some (List.assoc attr elem.attrs)
  with
    Not_found -> None

(** Helper functions. *)

let iter fn node tag =
  let rec loop = function
    | [] -> ()
    | (node::xs) -> (
      if node.tag = tag then fn node else ();
      loop xs
    ) in
  loop node.child_nodes;;

let fold_left fn init node tag =
  let fn2 m elem = if elem.tag = tag then fn m elem else m in
  List.fold_left fn2 init node.child_nodes

let map fn node tag =
  let rec loop = function
    | [] -> []
    | (node::xs) -> if node.tag = tag then (fn node) :: loop xs else loop xs in
  loop node.child_nodes

let find pred node = List.find pred node.child_nodes;;
