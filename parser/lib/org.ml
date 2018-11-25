open Format
open Tracks
open Re

type properties =
  {index: int; merged: record_mapping; differences: property_difference list}

type mapping_diff = {file: record_mapping list; properties: properties list}

let print_property_string ppf key value =
  fprintf ppf "@[<h>%s%s%s@[<h 2>%s@]@]@." ":" key ":    " value

let print_property_string_list ppf key values =
  let value = String.concat "; " values in
  print_property_string ppf key value

let print_header ppf mapping =
  let _, props = mapping in
  let extension = String.uppercase_ascii props.extension in
  match props with
  | {vinyl= true; _} ->
      extension |> Format.fprintf ppf "* %s Files with VINYL@."
  | {vinyl= false; _} ->
      extension |> Format.fprintf ppf "* %s Files without VINYL@."

let print_title ppf title = fprintf ppf "@[<h>%s%s@]@." "** " title

let print_record ppf title record =
  let p f = f ppf in
  p print_title title ;
  fprintf ppf ":PROPERTIES:@." ;
  p print_property_string "Author" record.author ;
  print_endline record.author;
  p print_property_string "Author+" record.features ;
  p print_property_string "Title" record.title ;
  p print_property_string_list "Title+" record.title_plus ;
  p print_property_string "Version" record.version ;
  p print_property_string_list "Version+" record.version_plus ;
  fprintf ppf ":END:@."

let print_group ppf group =
  print_header ppf @@ List.nth group 0 ;
  List.iter (fun (title, record) -> print_record ppf title record) group

let print_groups ppf groups =
  List.iter (fun group -> print_group ppf group) groups

let filter_properties_tag raw =
  List.filter (fun entry -> entry <> ":PROPERTIES:") raw

let remove_tag raw =
  let tag = Re.Perl.re ":(.*):    " |> Re.Perl.compile in
  let delim = Re.Perl.re ";" |> Re.Perl.compile in
  let values = Re.split tag raw in
  if List.length values = 0 then [""] else List.nth values 0 |> Re.split delim

let parse_entry entry =
  let name =
    List.nth entry 0 |> fun raw -> String.sub raw 3 (String.length raw - 3)
  in
  let nth_property n = List.nth entry n |> remove_tag in
  let plain_property n = List.nth (nth_property n) 0 in
  let empty_record = Tracks.empty in
  ( name
  , { empty_record with
      author= plain_property 1
    ; features= plain_property 2
    ; title= plain_property 3
    ; title_plus= nth_property 4
    ; version= plain_property 5
    ; version_plus= nth_property 6 } )

let parse_group headers group =
  let extension, vinyl = headers in
  let update_with_header (name, record) =
    (name, {record with extension; vinyl})
  in
  let entry_delim = Perl.re ":END:\n" |> Perl.compile in
  let property_delim = Perl.re "\n" |> Perl.compile in
  split entry_delim group
  |> List.map (fun entry ->
         split property_delim entry |> filter_properties_tag |> parse_entry
         |> update_with_header )

let extract_headers raw =
  let text_to_vinyl text = match text with "with" -> true | _ -> false in
  let header_regexp = Perl.re "\\* (.*) Files (.*) VINYL" |> Perl.compile in
  let occurrences = Re.all header_regexp raw in
  List.map
    (fun substrings ->
      match Group.all substrings with
      | [|_; ftype; vinyl|] ->
          (String.lowercase_ascii ftype, text_to_vinyl vinyl)
      | _ -> ("UNKNOWN", false) )
    occurrences

let parse content =
  let headers = extract_headers content in
  let header_delim =
    Perl.re "\\* (.*) Files (with|without) VINYL\n" |> Perl.compile
  in
  let groups = Re.split header_delim content in
  List.map2 (fun header group -> parse_group header group) headers groups
  |> List.flatten

let mapping_differences m1 m2 =
  let tbl2 = CCHashtbl.of_list m2 in
  let rec loop differences entries i =
    match entries with
    | (name, record) :: rest ->
        if Hashtbl.mem tbl2 name then
          let r2 = Hashtbl.find tbl2 name in
          print_endline r2.author;
          let property_differences = differences_record record r2 in
          if List.length property_differences > 0 then
            let properties_entry =
              { index= i
              ; merged= (name, record)
              ; differences= differences_record record r2 }
            in
            loop
              { differences with
                properties= properties_entry :: differences.properties }
              rest (i + 1)
          else loop differences rest (i + 1)
        else
          loop
            {differences with file= (name, record) :: differences.file}
            rest (i + 1)
    | _ -> differences
  in
  loop {file= []; properties= []} m1 0

let patch_mapping mapping properties =
  let rec loop cur_mapping to_patch =
    match to_patch with
    | {index; merged; _} :: rest ->
        loop (CCList.set_at_idx index merged cur_mapping) rest
    | [] -> cur_mapping
  in
  loop mapping properties
