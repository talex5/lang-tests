let re_app_name = Str.regexp "^[^./\\\\:=;'\"][^/\\\\:=;'\"]*$";;

let lookup_app name =
  if Str.string_match re_app_name name 0 then
    Basedir.load_first_config ["0install.net"; "apps"; name]
  else
    None
;;
