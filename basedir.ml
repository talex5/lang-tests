open Support;;

(* TODO: check for root *)

(* TODO: ZEROINSTALL_PORTABLE_BASE *)

(* TODO: Windows *)

let re_path_sep = Str.regexp_string path_sep;;

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/"
;;

let get_path home_var dirs_var = function
  | [] -> failwith "No defaults!"
  | (default_home :: default_system) ->
    let user_dir =
      try Sys.getenv home_var
      with Not_found -> default_home in
    let system_dirs =
      try List.filter (fun x -> x <> "") (Str.split re_path_sep (Sys.getenv dirs_var))
      with Not_found -> default_system in
    user_dir :: system_dirs
;;

let xdg_data_dirs = get_path "XDG_DATA_HOME" "XDG_DATA_DIRS"
  [home +/ ".local/share"; "/usr/local/share"; "/usr/share"];;

let xdg_cache_dirs = get_path "XDG_CACHE_HOME" "XDG_CACHE_DIRS"
  [home +/ ".cache"; "/var/cache"];;

let xdg_config_dirs = get_path "XDG_CONFIG_HOME" "XDG_CONFIG_DIRS"
  [home +/ ".config"; "/etc/xdg"];;

let load_first resource search_path =
  let rel_path = String.concat Filename.dir_sep resource in
  let rec loop = function
    | [] -> None
    | (x::xs) ->
        let path = x +/ rel_path in
        if Sys.file_exists path then Some path else loop xs
  in loop search_path
;;

let load_first_config resource = load_first resource xdg_config_dirs;;
