open Markup

let file_to_signal_stream dest = Markup.file dest |> fst |> parse_xml |> signals

let fix_attributes attributes = 
  let rec loop cur fixed = 
    match cur with 
    | ( (url,"FILE"), title)::rest -> loop rest  (((url, "FILE"), "trashino.wav")::fixed)
    | ( (url,"TITLE"), title)::rest -> loop rest  (((url, "TITLE"), "trashino")::fixed)
    | ( (url,"ARTIST"), artist)::rest -> loop rest (((url, "ARTIST"), artist)::fixed)
    | some::rest -> loop rest (some::fixed) 
    | [] -> fixed in 
  loop attributes []

let fix_nml_element (element: signal) =
  match element with
  | `Start_element (("", "ENTRY"), attributes) -> `Start_element (("", "ENTRY"), fix_attributes @@ attributes)
  | `Start_element (("", "LOCATION"), attributes) -> `Start_element (("", "LOCATION"), fix_attributes @@ attributes)
  | _ ->  element

let patch_nml mapping dest = 
  let nml_signals = file_to_signal_stream dest in
  let byte_stream = map fix_nml_element nml_signals |> write_xml in 
  let tmp_name = dest ^ ".backup" in 
  to_file (dest ^ ".backup")  byte_stream;
  Sys.rename tmp_name dest;