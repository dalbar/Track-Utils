open Format
open Tracks
open Re

let print_property_string ppf key value =
  fprintf ppf "@[<h>%s%s%s@[<h 2>%s@]@]@." ":" key ":    " value

let print_property_string_list ppf key values =
  let value = String.concat "; " values in
  print_property_string ppf key value

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

let filter_properties_tag raw =
  List.filter (fun entry -> entry <> ":PROPERTIES:") raw

let remove_tag raw =
  let tag = Re.Perl.re ":*.:    " |> Re.Perl.compile in
  List.nth (Re.split tag raw) 0

let parse_entry_to_org_mapping entry =
  let name =
    List.nth entry 0 |> fun raw -> String.sub raw 0 (String.length raw)
  in
  let nth_property n = List.nth entry n |> remove_tag in
  ( name
  , [ ("author", nth_property 1)
    ; ("features", nth_property 2)
    ; ("title", nth_property 3)
    ; ("title_plus", nth_property 4)
    ; ("version", nth_property 5)
    ; ("version_plus", nth_property 6) ] )

let parse content =
  let entry_delim = Perl.re ":END:" |> Perl.compile in
  let property_delim = Perl.re "\n" |> Perl.compile in
  split entry_delim content
  |> List.map (fun entry ->
         split property_delim entry |> filter_properties_tag
         |> parse_entry_to_org_mapping )
