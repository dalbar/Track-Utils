open Track_utils_parser

type file_overview = {directories: string list; files: string list}

let concat_path path file =
  if path = "." then file else Filename.concat path file

let read_file_to_string src =
  let ic = open_in src in
  let rec loop acc =
    match input_line ic with
    | line -> loop @@ acc ^ line ^ "\n"
    | exception End_of_file -> String.trim acc
  in
  loop ""

let extract_dirs_and_files path input =
  Array.fold_left
    (fun {directories; files} cur_file ->
      let path_to_file = concat_path path cur_file in
      if Sys.is_directory path_to_file then
        {directories= path_to_file :: directories; files}
      else {directories; files= path_to_file :: files} )
    {directories= []; files= []}
    input

let extract_track_files files =
  let validExtensions = [".mp4"; ".mp3"; ".jpg"; ".wav"; ".wmv"] in
  let isTrackFile file = List.mem (Filename.extension file) validExtensions in
  List.filter isTrackFile files

let perist_key_record_mapping mapping_list dest =
  let oc = open_out_gen [Open_creat; Open_wronly] 0o777 dest in
  List.iter
    (fun (key, value) -> Printf.fprintf oc "%s: %s\n" key value)
    mapping_list ;
  close_out oc

let write_org_file grouped_mapping dest =
  let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o777 dest in
  let ppf = Format.formatter_of_out_channel oc in
  Org.print_groups ppf grouped_mapping
