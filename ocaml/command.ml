open Constants;;

let make elem = elem;;

let get_path command = Qdom.get_attribute_opt ("", "path") command;;

let get_runner elem =
  let get_runner runner = runner in
  match ZI.map get_runner elem "runner" with
    | [] -> None
    | [runner] -> Some runner
    | _ -> failwith "Multiple runners!"
;;

let get_elem command = command;;
