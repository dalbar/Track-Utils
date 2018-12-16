open Track_utils_shortener
open Track_utils_parser
open Track_utils_helpers.Io
open Track_utils_helpers.Ds
open Track_utils_parser.Tracks

let shortening_processing_pipe track_records concat_path1 =
  let track_shortened_map = Shortener.shorten_track_list track_records in
  let mapping_shortened_record = CCHashtbl.to_list track_shortened_map in
  let mapping_shortened_old =
    List.map
      (fun (key, record) -> (key, Tracks.stringify_token_record record))
      mapping_shortened_record
  in
  if List.length mapping_shortened_old > 0 then (
    perist_key_record_mapping mapping_shortened_old
    @@ concat_path1 ".track_utils" ;
    List.iter
      (fun (shortened, old) ->
        if Sys.file_exists (concat_path1 old) then
          Sys.rename (concat_path1 old) (concat_path1 shortened) )
      mapping_shortened_old ) ;
  mapping_shortened_record

let org_processing_pipe mapping dest =
  if List.length mapping > 0 then (
    let to_key (_, r) = (r.vinyl, r.extension) in
    let org_content = CCOpt.wrap read_file_to_string dest in
    let org_ds = CCOpt.get_or ~default:"" org_content |> Org.parse in
    let differences_org_to_mapping = Org.mapping_differences org_ds mapping in
    Org.print_warning differences_org_to_mapping dest ;
    let differences_mapping_to_org = Org.mapping_differences mapping org_ds in
    let fixed_missing_files = org_ds @ differences_mapping_to_org.file in
    let updated_records =
      Org.patch_mapping fixed_missing_files
        differences_mapping_to_org.properties
    in
    let grouped = group_by to_key updated_records in
    write_org_file grouped dest )
 
let history_processing_pipe files concat_cur_path =
  let dest = concat_cur_path ".track_utils" in
  if Sys.file_exists dest then
    let history = read_file_to_string dest in
    let hist_tbl = History.parse history in
    List.map
      (fun file ->
        let basename = Filename.basename file in
        if Hashtbl.mem hist_tbl basename then
          Hashtbl.find hist_tbl basename |> concat_cur_path
        else file )
      files
  else files

let reverting_processing_pipe shortened reverted path =
  let to_key (_, r) = (r.vinyl, r.extension) in
  let history =
    List.map2
      (fun shortened reverted -> (shortened, reverted))
      shortened reverted
    |> List.filter (fun (shortened, reverted) -> shortened <> reverted)
  in
  let history_file = concat_path path ".track_utils" in
  let org_file = concat_path path "db.org" in
  if Sys.file_exists history_file then (
    List.iter (fun (shortened, long) -> Sys.rename shortened long) history ;
    Sys.remove history_file ) ;
  if Sys.file_exists org_file then
    let org_content = CCOpt.wrap read_file_to_string org_file in
    let org_ds = CCOpt.get_or ~default:"" org_content |> Org.parse in
    let grouped = group_by to_key (Org.revert_mapping org_ds history) in
    write_org_file grouped org_file

let nml_processing_pipe mapping files = 
    let nml_files = List.filter (fun name ->  Filename.extension name = ".nml") files  in
    List.iter (fun name -> Nml.patch_nml mapping name) nml_files

let track_cli revert recurisve shorten org nml path =
  let rec loop cur_path dic_acc =
    let concat_cur_path file = concat_path cur_path file in
    let cur_files = Sys.readdir cur_path in
    let {directories; files} = extract_dirs_and_files cur_path cur_files in
    let track_files = extract_track_files files in
    let reverted_files = history_processing_pipe track_files concat_cur_path in
    if revert then
      reverting_processing_pipe track_files reverted_files cur_path ;
    let track_records = Tracks.parse_string_list reverted_files in
    let record_map =
      if shorten then shortening_processing_pipe track_records concat_cur_path
      else
        List.map2
          (fun record files -> (Filename.basename files, record))
          track_records track_files
    in
    if org then org_processing_pipe record_map (concat_cur_path "db.org");
    if nml then nml_processing_pipe record_map files;
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

let revert =
  let doc = "Undo shortening operations." in
  Arg.(value & flag & info ["u"] ~doc)

let nml =
  Arg.(
    value & flag
    & info ["u"; "nml"] ~docv:"u" ~doc:"Patch NML File with org file")
  
let cmd =
  ( Term.(const track_cli $ revert $ recursive $ shorten $ org $nml $ path)
  , Term.info "track-utils" )

let () = Term.(exit @@ eval cmd)
