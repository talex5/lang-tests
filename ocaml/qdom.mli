(** XML processing. *)

(** An XML element node (and nearby text). *)
type element = {
  tag: Xmlm.name;
  mutable attrs: Xmlm.attribute list;
  mutable child_nodes: element list;
  mutable text_before: string;        (** The text node immediately before us *)
  mutable last_text_inside: string;   (** The last text node inside us with no following element *)
};;

exception InvalidXML of string

(** {2 Parsing} *)

(** @raise InvalidXML if the XML is not well formed. *)
val parse_input : Xmlm.input -> element

(** @raise InvalidXML if the XML is not well formed. *)
val parse_file : string -> element

(** {2 Accessing the tree} *)

val get_attribute : Xmlm.name -> element -> string
val get_attribute_opt : Xmlm.name -> element -> string option

(** {2 Helper functions} *)

val iter : (element -> unit) -> element -> Xmlm.name -> unit

(** [fold_left fn init elem tag] processes each child of [elem] with the given [tag] using [fn]. *)
val fold_left : ('a -> element -> 'a) -> 'a -> element -> Xmlm.name -> 'a

val map : (element -> 'a) -> element -> Xmlm.name -> 'a list

val find : (element -> bool) -> element -> element
