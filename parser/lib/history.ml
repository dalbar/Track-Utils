open Re

let parse history = 
  let newline_regexp = Perl.compile @@ Perl.re "\n" in 
  let delim_regexp = Perl.compile @@ Perl.re ": " in 
  let mappings = split newline_regexp history in 
  List.map (fun mapping -> split delim_regexp mapping) mappings