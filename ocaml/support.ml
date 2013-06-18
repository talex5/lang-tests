module StringMap = Map.Make(String);;

type filepath = string;;
type varname = string;;

let with_open file fn =
  let ch = open_in file in
  let result = try fn ch with exn -> close_in ch; raise exn in
  let () = close_in ch in
  result
;;

let default d = function
  | None -> d
  | Some x -> x;;

let rec first_match fn = function
  | [] -> None
  | (x::xs) -> match fn x with
      | Some _ as result -> result
      | None -> first_match fn xs;;

let path_sep = if Filename.dir_sep = "/" then ":" else ";";;

let (+/) : filepath -> filepath -> filepath = Filename.concat;;

let rec makedirs path mode =
  try (
    if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then ()
    else failwith ("Not a directory: " ^ path)
  ) with Unix.Unix_error _ -> (
    let parent = (Filename.dirname path) in
    assert (path <> parent);
    makedirs parent mode;
    Unix.mkdir path mode
  )
;;

let abspath path =
  if path.[0] = '/' then path
  else (Sys.getcwd ()) +/ path
;;
