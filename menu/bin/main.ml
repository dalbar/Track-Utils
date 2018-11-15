
type file_overview = { directories:string list; files:string list }
let extract_dirs_and_files path input =
  Array.fold_left 
    (fun {directories; files} cur_file ->
      let path_to_file = (Filename.concat path cur_file) in
      if (Sys.is_directory path_to_file) then 
        {directories = path_to_file::directories; files} 
      else {directories; files = path_to_file::files}) {directories=[]; files=[]} input

let rec ls recurisve shorten path = 
  let rec_with_path new_poth = ls recurisve shorten new_poth in
  let cur_files = Sys.readdir path in
  let { directories; files } = extract_dirs_and_files path cur_files in
  if shorten then begin
      List.iter print_endline directories;
      List.iter print_endline files;
      print_endline "------";
  end;
  if recurisve then List.iter (fun new_dir -> rec_with_path new_dir) directories else ()


open Cmdliner


let path = Arg.(value & pos 0 string "." & info [] ~docv:"PATH")


let shorten = Arg.(value & flag & info ["s"; "shorten"] ~docv:"N" ~doc:"Shorten all files in the target directory")


let recursive =
  let doc = "Execute operations recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let cmd = 
  Term.(const ls $ recursive $ shorten $ path),
  Term.info "ls"

let () = Term.(exit @@ eval cmd)