module Track_Tokens = struct
  type path_delimiter = Slash | Backshlash

  type delimiter =
    | Slash
    | Backslash
    | OpeningCurlyBracket
    | OpeningSquareBracket
    | ClosingCurlyBracket
    | ClosingSquareBracket
    | AuthorTitleDelimiter

  let delimiter_to_string dl =
    match dl with
    | Slash -> "/"
    | Backslash -> "\\"
    | OpeningCurlyBracket -> "("
    | ClosingCurlyBracket -> ")"
    | OpeningSquareBracket -> "["
    | ClosingSquareBracket -> "["
    | AuthorTitleDelimiter -> "-"

  type prefix =
    | Vinyl

  let prefix_to_string p =
    match p with
    | Vinyl -> "v"

  type feature_operator =
    | Feature
    | Ft
    | And

  let feature_operator_to_string op =
    match op with
    | Feature -> "feature"
    | Ft -> "ft"
    | And -> "&"

  type operator =
    | Dot
    | Feature of feature_operator

  let operator_to_string op =
    match op with
    | Dot -> "."
    | Feature ft_op -> feature_operator_to_string ft_op

  type track_token =
    | Operator of operator
    | Word of string
    | Prefix of prefix
    | Delimiter of delimiter

  let flush_buffer buffer =
    let content = Buffer.contents buffer in
    Buffer.clear buffer;
    content

  let cons_non_empty_buffer_as_word buffer tokens =
    let tmp_acc = if Buffer.length buffer = 0 then tokens else
        (Word (flush_buffer buffer))::tokens in
    tmp_acc

  let buffer_prev_element buffer position =
    Buffer.nth buffer (position-1)

  let buffer_next_element buffer position =
    Buffer.nth buffer (position+1)

  let track_tokenizer track_buffer =
    let buf_len = Buffer.length track_buffer in
    let tmp_buffer = Buffer.create buf_len in
    Buffer.add_string tmp_buffer (Buffer.contents track_buffer);
    Buffer.add_char tmp_buffer '\n';
    let rec loop tracks tokens word_acc cur_position =
      try
        let cur_char = Buffer.nth tmp_buffer cur_position in
        let next_word_acc = word_acc ^ Char.escaped cur_char in
        let cur_tokens = Word word_acc::tokens in
        match cur_char with
        | 'v' -> if buffer_next_element track_buffer cur_position = '_' then begin
            loop tracks (Prefix Vinyl::tokens) "" (cur_position+2)
          end
          else begin
            loop tracks tokens next_word_acc (cur_position + 1)
          end
        | '('  ->
          loop tracks (Delimiter OpeningCurlyBracket::cur_tokens) "" (cur_position + 1)
        | '[' ->
          loop tracks (Delimiter OpeningCurlyBracket::cur_tokens) "" (cur_position + 1)
        | ')' ->
          loop tracks (Delimiter ClosingCurlyBracket::cur_tokens) "" (cur_position + 1)
        | ']' ->
          loop tracks (Delimiter ClosingSquareBracket::cur_tokens) "" (cur_position + 1)
        | '/' ->
          loop tracks (Delimiter Slash::cur_tokens) "" (cur_position + 1)
        | ' ' ->
          if buffer_next_element tmp_buffer cur_position = '-'
          && buffer_next_element tmp_buffer (cur_position + 1) = ' ' then begin
            loop tracks (Delimiter AuthorTitleDelimiter::cur_tokens) "" (cur_position + 3)
          end
          else begin
            loop tracks cur_tokens "" (cur_position + 1)
          end
        | '.' ->
          loop tracks (Operator Dot::cur_tokens)  "" (cur_position + 1)
        | '\n' ->
          loop (List.rev cur_tokens::tracks) [] "" (cur_position+1)
        | _ -> loop tracks tokens next_word_acc (cur_position + 1)
      with Invalid_argument _ ->
        (List.rev tokens)::tracks
    in loop [] [] "" 0


end
module Track = struct
  type extensions = | WAV | MP3 | MP4
  type t = string
  let extension_to_string ext =
    match ext with
    | WAV -> "wav"
    | MP3 -> "mp3"
    | MP4 -> "mp4"

  let year = "3000"
  let author = ""
  let author_plus : string list = []
  let title = ""
  let title_plus : string list = []
  let version = ""
  let version_plus: string list = []
  let format = ""
  let extension = WAV

  let compare k1 k2 = if k1 = k2 then 1 else 0

end

module Track_Map = CCMap.Make(Track)

let reduce_string_list s_list =
  List.fold_left (fun x y -> x ^ y) "" s_list
let parse_track_tokens tokens =
  let rec loop tokens track_map acc =
    match tokens with
    | [] -> track_map
    | (Track_Tokens.Word "r")::rest -> loop rest track_map ("r"::acc)
    | _ ->
      let tmp_map = Track_Map.add "path" (reduce_string_list acc) track_map in
      loop [] tmp_map []
  in
  loop tokens Track_Map.empty []
