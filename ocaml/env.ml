module M = Map.Make(String);;

type env = string M.t;;

let re_equals = Str.regexp_string "=";;

let copy_current_env () : env =
  let parse_env env line =
    match Str.bounded_split_delim re_equals line 2 with
    | [key; value] -> M.add key value env
    | _ -> failwith (Printf.sprintf "Invalid environment mapping '%s'" line)
  in Array.fold_left parse_env M.empty (Unix.environment ())
;;

let putenv name value env =
  (* Printf.fprintf stderr "Adding: %s=%s\n" name value; *)
  M.add name value env
;;

let find name env =
  M.find name env
;;

let find_opt name env =
  try Some (M.find name env)
  with Not_found -> None
;;

let to_array env =
  let to_item (key, value) = key ^ "=" ^ value in
  Array.of_list (List.map to_item (M.bindings env))
;;
