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

let extract_track_files files =
  let validExtensions = [".mp4"; ".mp3"; ".jpg"; ".wav"; ".wmv"] in
  let isTrackFile file = List.mem (Filename.extension file) validExtensions in
  List.filter isTrackFile files

let extract_track_files_from_dir dir parent = 
  Sys.readdir dir |> Array.to_list |> extract_track_files |> List.map (fun n -> parent ^ "/" ^ n)

let extract_dirs_and_track_files path input =
  Array.fold_left
    (fun {directories; files} cur_file ->
      let path_to_file = concat_path path cur_file in
      if Sys.is_directory path_to_file then
        let new_files = extract_track_files_from_dir path_to_file cur_file in
        {directories= path_to_file :: directories; files = new_files @ files}
      else {directories; files}
    )
    {directories= []; files= []}
    input

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
