let get_interface runner = Qdom.get_attribute ("", "interface") runner;;

let get_command_name runner = match Qdom.get_attribute_opt ("", "name") runner with
| None -> "run"
| Some name -> name;;

let get_elem runner = runner;;
