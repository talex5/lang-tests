let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> failwith "usage: runsels selections.xml arg..."
  | (sels_path :: args) ->
    let doc = Qdom.parse_file sels_path in
    let selections = Selections.make doc in
    let stores = ["/home/tal/.cache/0install.net/implementations"; "/var/cache/0install.net/implementations"] in
    Run.execute_selections selections args stores;;
