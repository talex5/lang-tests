(** The main executable. *)

open Support;;

let () =
  try
    let config = Config.get_default_config () in
    match List.tl (Array.to_list Sys.argv) with
    | [] -> failwith "usage: runsels selections.xml arg..."
    | (app_or_sels :: args) ->
        let sels_path = match Apps.lookup_app app_or_sels config with
        | None -> app_or_sels
        | Some app_path -> app_path +/ "selections.xml" in
        let sels = Qdom.parse_file sels_path in
        Run.execute_selections sels args config
  with
  | ex ->
      output_string stderr (Printexc.to_string ex);
      output_string stderr "\n";
      if not (Printexc.backtrace_status ()) then
        output_string stderr "(hint: run with OCAMLRUNPARAM=b to get a stack-trace)\n"
      else
        Printexc.print_backtrace stderr;
      exit 1
;;
