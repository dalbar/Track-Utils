open Track_utils_parser.Parser

let special_symbol = '_'
let cut_version_plus track_details special_symbol = 
  { track_details with title_plus = [special_symbol]}

let cut_title_plus track_details special_symbol =
  { track_details with title_plus = [special_symbol]}

let cut_features track_details special_symbol = 
  { track_details with features = [special_symbol]}

let shorten_track track_details special_symbol =
  let cut f list = f list special_symbol in 
  cut cut_features @@ cut cut_title_plus @@ cut cut_version_plus track_details