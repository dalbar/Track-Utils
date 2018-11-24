open Format
open Tracks
open Re

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
  p print_property_string "Author+" record.features ;
  p print_property_string "Title" record.title ;
  p print_property_string_list "Title+" record.title_plus ;
  p print_property_string "Version" record.version ;
  p print_property_string_list "Version+" record.version_plus ;
  fprintf ppf ":END:@."

let print_group ppf group =
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
  printf "%d %s" (List.length entry) "----------\n" ;
  let name =
    List.nth entry 0 |> fun raw -> String.sub raw 0 (String.length raw)
  in
  let nth_property n = List.nth entry n |> remove_tag in
  let plain_property n = List.nth (nth_property n) 0 in
  let empty_record = Tracks.empty in
  ( name
  , { empty_record with
      author= plain_property 0
    ; features= plain_property 1
    ; title= plain_property 2
    ; title_plus= nth_property 3
    ; version= plain_property 4
    ; version_plus= nth_property 5 } )

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
