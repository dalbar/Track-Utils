open Re

let parse ?(inv = false) history =
  let newline_regexp = Perl.compile @@ Perl.re "\n" in
  let delim_regexp = Perl.compile @@ Perl.re ": " in
  let list_pair_to_tuple pair =
    if inv then (List.nth pair 1, List.nth pair 0)
    else (List.nth pair 0, List.nth pair 1)
  in
  let mappings = split newline_regexp history in
  List.map
    (fun mapping -> split delim_regexp mapping |> list_pair_to_tuple)
    mappings
  |> CCHashtbl.of_list
