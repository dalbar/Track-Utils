
open Track_utils_shortener
open Track_utils_parser

type file_overview = { directories:string list; files:string list }

let concat_path path file = if path = "." then file else Filename.concat path file

let extract_dirs_and_files path input =
  Array.fold_left 
    (fun {directories; files} cur_file ->
      let path_to_file = concat_path path cur_file in
      if (Sys.is_directory path_to_file) then 
        {directories = path_to_file::directories; files} 
      else {directories; files = path_to_file::files}) {directories=[]; files=[]} input

let extract_track_files files =
  let validExtensions = [".mp4"; ".mp3"; ".jpg"; ".wav"; ".wmv"] in
  let isTrackFile file = List.mem (Filename.extension file) validExtensions in
  List.filter isTrackFile files

let perist_key_record_mapping mapping_list dest = 
  let oc = open_out_gen [Open_creat; Open_append] 0o777 dest in 
  List.iter (fun (key, value) -> if key <> value then Printf.fprintf oc "%s: %s\n" key value;) mapping_list;
  close_out oc

let track_cli recurisve shorten path = 
  let rec loop cur_path dic_acc = 
    let concat_cur_path file = concat_path cur_path file in
    let cur_files = Sys.readdir cur_path in
    let { directories; files } = extract_dirs_and_files cur_path cur_files in
    if shorten then begin
        let track_files = extract_track_files files in 
        if List.length track_files > 0 then begin
          let track_records = Parser.parse_string_list track_files in
          let track_shortened_map = Shortener.shorten_track_list track_records in 
          let mapping_shortened_record = CCHashtbl.to_list track_shortened_map in
          let mapping_shortened_old = List.map (fun (key, record) -> (key,  Parser.stringify_token_record record)) mapping_shortened_record in
          perist_key_record_mapping mapping_shortened_old @@ concat_cur_path ".track_utils";
          List.iter 
          (fun (shortened, old) -> Sys.rename (concat_cur_path old)  (concat_cur_path shortened)) mapping_shortened_old
        end;
    end;
    if recurisve then begin
      match directories @ dic_acc with 
      | [] -> ()
      | hd::rest -> loop hd rest;
      end; in
  loop path []

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