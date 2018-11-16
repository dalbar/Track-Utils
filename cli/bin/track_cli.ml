
open Track_utils_shortener
open Track_utils_parser

type file_overview = { directories:string list; files:string list }

let extract_dirs_and_files path input =
  Array.fold_left 
    (fun {directories; files} cur_file ->
      let path_to_file = (Filename.concat path cur_file) in
      if (Sys.is_directory path_to_file) then 
        {directories = path_to_file::directories; files} 
      else {directories; files = path_to_file::files}) {directories=[]; files=[]} input

let extract_track_files files =
  let validExtensions = [".mp4"; ".mp3"; ".jpg"; ".wav"; ".wmv"] in
  let isTrackFile file = List.mem (Filename.extension file) validExtensions in
  List.filter isTrackFile files
let rec track_cli recurisve shorten path = 
  let rec_with_path new_poth = track_cli recurisve shorten new_poth in
  try
    let cur_files = Sys.readdir path in
    let { directories; files } = extract_dirs_and_files path cur_files in
    if shorten then begin
       let track_files = extract_track_files files in 
       if List.length track_files > 0 then begin
       let track_records = Parser.parse_string_list track_files in
       let track_shortened_map = Shortener.shorten_track_list track_records in 
       List.iter print_endline (CCHashtbl.keys_list track_shortened_map)
       end;
    end;
    if recurisve then List.iter (fun new_dir -> rec_with_path new_dir) directories else ()
  with | Sys_error e -> Printf.fprintf stderr "The following error occured\n: %s"  e 

open Cmdliner

let path = Arg.(value & pos 0 string "." & info [] ~docv:"PATH")


let shorten = Arg.(value & flag & info ["s"; "shorten"] ~docv:"N" ~doc:"Shorten all files in the target directory")


let recursive =
  let doc = "Execute operations recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let cmd = 
  Term.(const track_cli $ recursive $ shorten $ path),
  Term.info "ls"

let () = Term.(exit @@ eval cmd)