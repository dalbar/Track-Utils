open Track_utils_shortener
open Track_utils_parser
open Track_utils_helpers.Io

let track_cli recurisve shorten org path =
  let rec loop cur_path dic_acc =
    let concat_cur_path file = concat_path cur_path file in
    let cur_files = Sys.readdir cur_path in
    let {directories; files} = extract_dirs_and_files cur_path cur_files in
    let track_files = extract_track_files files in
    let track_records = Tracks.parse_string_list track_files in
    let record_map =
      if shorten then (
        let track_shortened_map = Shortener.shorten_track_list track_records in
        let mapping_shortened_record = CCHashtbl.to_list track_shortened_map in
        let mapping_shortened_old =
          List.map
            (fun (key, record) -> (key, Tracks.stringify_token_record record))
            mapping_shortened_record
        in
        let changes =
          List.filter
            (fun (key, record) -> key <> record)
            mapping_shortened_old
        in
        if List.length changes > 0 then (
          perist_key_record_mapping changes @@ concat_cur_path ".track_utils" ;
          List.iter
            (fun (shortened, old) ->
              Sys.rename (concat_cur_path old) (concat_cur_path shortened) )
            changes ) ;
        mapping_shortened_record )
      else
        List.map
          (fun record -> (Tracks.stringify_token_record record, record))
          track_records
    in
    if org && List.length record_map > 0 then
      write_org_file record_map @@ concat_cur_path "db.org" ;
    if recurisve then
      match directories @ dic_acc with [] -> () | hd :: rest -> loop hd rest
  in
  loop path []

open Cmdliner

let path = Arg.(value & pos 0 string "." & info [] ~docv:"PATH")

let shorten =
  Arg.(
    value & flag
    & info ["s"; "shorten"] ~docv:"N"
        ~doc:"Shorten all files in the target directory")

let org =
  Arg.(
    value & flag
    & info ["t"; "org"] ~docv:"t" ~doc:"Create Org File of all Tracks")

let recursive =
  let doc = "Execute operations recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let cmd =
  (Term.(const track_cli $ recursive $ shorten $ org $ path), Term.info "ls")

let () = Term.(exit @@ eval cmd)
