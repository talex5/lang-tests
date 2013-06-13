let with_open file fn =
  let ch = open_in file in
  let result = try fn ch with exn -> close_in ch; raise exn in
  let () = close_in ch in
  result
;;

let default d = function
  | None -> d
  | Some x -> x;;

let expect opt msg = match opt with
  | Some x -> x
  | None -> failwith msg;;

let rec first_match fn = function
  | [] -> None
  | (x::xs) -> match fn x with
      | Some _ as result -> result
      | None -> first_match fn xs;;

let starts_with str prefix =
  let ls = String.length str in
  let lp = String.length prefix in
  if lp > ls then false else
    let rec loop i =
      if i = lp then true
      else if str.[i] <> prefix.[i] then false
      else loop (i + 1)
    in loop 0;;

let path_sep = if Filename.dir_sep = "/" then ":" else ";";;

(* Look up an environment variable in an array from Unix.environment.
   Maybe we should convert it to a Map instead? *)
let getenv_opt name env =
  let prefix = name ^ "=" in
  let rec loop = function
    | [] -> None
    | x::xs ->
        if starts_with x prefix then
          let pl = String.length prefix in
          let xl = String.length x in
          Some (String.sub x pl (xl - pl))
        else loop xs in
  loop env
;;

let getenv name env = match getenv_opt name env with
  | Some x -> x
  | None -> failwith ("Variable '" ^ name ^ "' not set")
;;

let (+/) = Filename.concat;;
