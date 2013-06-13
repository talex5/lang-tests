open Support;;

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> failwith "usage: runsels selections.xml arg..."
  | (app_or_sels :: args) ->
      let sels_path = match Apps.lookup_app app_or_sels with
      | None -> app_or_sels
      | Some app_path -> app_path +/ "selections.xml" in
      let doc = Qdom.parse_file sels_path in
      let selections = Selections.make doc in
      Run.execute_selections selections args Stores.default_stores;;
