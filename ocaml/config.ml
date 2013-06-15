type config = {
  basedirs: Basedir.config;
  stores: string list;
  resource_dir: string;
};;

let get_default_config () =
  let basedirs_config = Basedir.get_default_config () in {
    basedirs = basedirs_config;
    stores = Stores.get_default_stores basedirs_config;
    resource_dir = Filename.dirname (Support.abspath Sys.argv.(0))
  }
;;
