exception Runenv_failure of string;;

let getenv name =
  try Sys.getenv name
  with Not_found -> raise (Runenv_failure ("Environment variable '" ^ name ^ "' not set"))
;;

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | arg0::args ->
    (* let () = print_endline("begin... argv[0] = " ^ arg0) in *)
    let var = "0install-runenv-" ^ Filename.basename arg0 in
    (* let s = try Sys.getenv var with x -> failwith("Var not found " ^ var) in *)
    let s = getenv var in
    let open Yojson.Basic in
    let envargs = Util.convert_each Util.to_string (from_string s) in
    Unix.execv (List.hd envargs) (Array.of_list (envargs @ args))
