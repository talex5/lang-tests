open Constants;;

let make elem = elem;;

let get_path command = Qdom.get_attribute_opt ("", "path") command;;

let get_runner elem =
  let get_runner runner = runner in
  match Qdom.map get_runner elem (xmlns_feed, "runner") with
    | [] -> None
    | [runner] -> Some runner
    | _ -> failwith "Multiple runners!"
;;

let get_elem command = command;;
