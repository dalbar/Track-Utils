open Track_utils_parser.Parser

let special_symbol = "_"
let cut_version_plus track_details special_symbol = 
  { track_details with version_plus = [special_symbol]}

let cut_title_plus track_details special_symbol =
  { track_details with title_plus = [special_symbol]}

let cut_features track_details special_symbol = 
  { track_details with features = special_symbol}

let cut_conjunctive_authors track_details special_symbol = 
  let split_conjunction = Re.Perl.compile (Re.Perl.re "\\s*(&|vs)\\s*") in
  let authors = Re.split split_conjunction track_details.author in
  { track_details with author = (List.nth authors 0) ^ special_symbol }

let title_to_initials track_details = 
  let words = Re.split (Re.compile (Re.Perl.re ("\\s\\s*"))) track_details.title in 
  let initials = List.map (fun w -> print_endline w; String.uppercase_ascii (String.sub w 0 1) ) words in
  { track_details with title = String.concat "." initials }

let reduce_version track_details special_symbol = 
  let words = Re.split (Re.compile (Re.Perl.re ("\\s*"))) track_details.title in
  let mix = Re.compile (Re.Perl.re "Mix") in 
  let remix = Re.compile (Re.Perl.re "Remix") in 
  let version = Re.compile (Re.Perl.re "Version") in
  let new_version str = List.nth words 1 ^ special_symbol ^ str in
  let matches_once regexp = List.length (Re.matches regexp track_details.version) > 0 in 
  
  if matches_once mix then { track_details with version = new_version "Mix" } 
  else if matches_once remix then { track_details with version = new_version "Remix"}
  else if matches_once version then { track_details with version = new_version "Version"}
  else track_details

type shortening_operation = 
  | Author
  | Title
  | Version
let shorten_track  ?(special_symbol = special_symbol) ?(treshhold = 85) track_details =
  let cut f record = f record special_symbol in
  let cur_res = cut cut_features @@ cut cut_title_plus @@ cut cut_version_plus track_details in
  let rec loop record cur_phase =
    let cur_as_string = stringify_token_record record in 
    if String.length cur_as_string <= treshhold then record 
    else match cur_phase with 
      | Author -> loop (cut cut_conjunctive_authors record) Title
      | Title -> loop (title_to_initials record) Version
      | Version -> record in
  loop cur_res Author

let shorten_track_list ?(special_symbol = special_symbol) ?(treshhold = 85) record_list =
  let track_map = CCHashtbl.of_list [] in
  let has_duplicate name = Hashtbl.mem track_map name in
  let resolve_conflict record counter =  { record with extension= string_of_int counter ^ "." ^ record.extension} in
  let shorten record = 
    let shortened = shorten_track ~special_symbol ~treshhold record in 
    let rec loop cur_record counter = 
      let stringified = stringify_token_record cur_record in 
      if has_duplicate stringified then begin 
        let num_duplicates = counter + 1 in 
        loop (resolve_conflict shortened num_duplicates) num_duplicates
      end
      else  Hashtbl.add track_map stringified record in 
    loop shortened 0 in
  List.iter shorten record_list;
  track_map