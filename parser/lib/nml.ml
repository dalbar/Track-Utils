open Markup

type attributes = (name * string) list

type xml = Text of string | Element of name * attributes * xml list

exception Wrong_Tag of string

exception Empty_NML of string

let file_to_signal_stream dest =
  Markup.file dest |> fst |> parse_xml |> signals

let signal_stream_to_file stream dest =
  let byte_stream = stream |> write_xml in
  let tmp_name = dest ^ ".backup" in
  to_file (dest ^ ".backup") byte_stream ;
  Sys.rename tmp_name dest

let tree_to_stream tree =
  from_tree
    (fun element ->
      match element with
      | Text s -> `Text s
      | Element (name, atts, children) -> `Element (name, atts, children) )
    tree

let stream_to_xml stream =
  tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(fun name atts children -> Element (name, atts, children))
    stream

let attibutes_to_hashtbl attributes = CCHashtbl.of_list attributes

let deconstruct_xml_element element =
  match element with
  | Text _ -> raise (Wrong_Tag "Not a Location")
  | Element (name, atts, children) -> (name, atts, children)

let patch_entry element history_tbl mapping cur_path =
  let name, atts, children = deconstruct_xml_element element in
  let is_location xml =
    match xml with
    | Text _ -> false
    | Element ((_, tag), _, _) -> tag = "LOCATION"
  in
  let location_list, other_elements = List.partition is_location children in
  if List.length location_list > 0 then
    let l_name, l_atts, l_children =
      List.nth location_list 0 |> deconstruct_xml_element
    in
    let deconstructed = attibutes_to_hashtbl l_atts in
    let filename =
      CCHashtbl.get_or deconstructed ("", "Location") ~default:""
    in
    let is_equal (track_name, _) =
      track_name = filename
      || CCHashtbl.get_or history_tbl track_name ~default:"" = filename
    in
    let matching = CCList.find_opt is_equal mapping in
    if CCOpt.is_some matching then (
      let shortened, record = CCOpt.get_exn matching in
      Hashtbl.replace deconstructed ("", "FILE") shortened ;
      Hashtbl.replace deconstructed ("", "DIR") cur_path ;
      let new_location =
        Element (l_name, CCHashtbl.to_list deconstructed, l_children)
      in
      let element_atts = attibutes_to_hashtbl atts in
      Hashtbl.replace element_atts ("", "ARTIST") record.Tracks.author ;
      Hashtbl.replace element_atts ("", "TITLE") record.Tracks.title ;
      Element
        (name, CCHashtbl.to_list element_atts, new_location :: other_elements) )
    else element
  else element

let patch_entries tree patch =
  let rec loop cur_element =
    match cur_element with
    | Text s -> Text s
    | Element ((url, "ENTRY"), attributes, children) ->
        patch @@ Element ((url, "ENTRY"), attributes, children)
    | Element (name, attributes, children) ->
        Element (name, attributes, List.map loop children)
  in
  loop tree

let patch_nml history mapping path dest =
  let xml_tree = file_to_signal_stream dest |> trim |> stream_to_xml in
  let patch entry = patch_entry entry history mapping path in
  let write_to_file tree = signal_stream_to_file tree dest in
  match xml_tree with
  | None -> raise (Empty_NML "nml file is empty")
  | Some tree ->
      patch_entries tree patch |> tree_to_stream |> pretty_print
      |> write_to_file
