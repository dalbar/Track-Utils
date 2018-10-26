module Track_Tokens = struct
  type path_delimiter = Slash | Backshlash

  type delimiter =
    | Slash
    | OpeningCurlyBracket
    | OpeningSquareBracket
    | ClosingCurlyBracket
    | ClosingSquareBracket
    | AuthorTitleDelimiter

  let delimiter_to_string dl =
    match dl with
    | Slash -> "/"
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

  let token_to_string token =
    match token with
    | Operator o -> operator_to_string o
    | Word word -> word
    | Prefix p -> prefix_to_string p
    | Delimiter d -> delimiter_to_string d

  let track_tokenizer track_buffer =
    let rec loop tracks tokens word_acc cur_position =
      try
        let cur_char = Buffer.nth track_buffer cur_position in
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
          if buffer_next_element track_buffer cur_position = '-'
          && buffer_next_element track_buffer (cur_position + 1) = ' ' then begin
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
        tracks
    in loop [] [] "" 0


end
module Track_Table = struct
  type extensions = | WAV | MP3 | MP4

  let inital_key_values = [
    "author", "";
    "features", "";
    "vinyl", "no";
    "title", "";
    "title_plus", "";
    "version", "";
    "version_plus", "";
    "extensions", "";
    "year", ""
  ]

  let empty = CCHashtbl.Poly.of_list(inital_key_values)
end


open Track_Tokens

type track_block =
  | Year
  | AuthorAndPrefix
  | Title
  | Version
  | Extension

let parse_track_tokens tokens =
  let track_table = Track_Table.empty in
  let inc_bracket_depth depth = if depth < 2 then depth + 1 else 22 in
  let dec_bracket_depth depth = if depth > 0 then depth - 1 else 0 in
  let rec loop tokens word_acc bracket_depth track_block  =
    let cur_info = String.concat " " word_acc in
    match tokens, track_block with
    | [], AuthorAndPrefix ->
      Hashtbl.replace track_table "author" cur_info;
    | [], Year -> Hashtbl.replace track_table "year" cur_info;
    | [], _ -> ()
    | (Operator Dot)::rest, Extension ->
      Hashtbl.replace track_table "extensions" cur_info;
      loop rest [] bracket_depth Extension
    | (Operator _)::rest, _ -> loop rest word_acc bracket_depth track_block
    | (Prefix Vinyl)::rest, _ ->
      Hashtbl.replace track_table "author" cur_info;
      Hashtbl.replace track_table "vinyl" "yes";
      loop rest [] bracket_depth Version
    | (Delimiter Slash)::rest, AuthorAndPrefix ->
      Hashtbl.replace track_table "author" cur_info;
      loop rest [] bracket_depth Year
    | (Delimiter AuthorTitleDelimiter)::rest, _ ->
      Hashtbl.replace track_table "title" cur_info;
      loop rest [] 0 AuthorAndPrefix
    | (Delimiter ClosingCurlyBracket  |  Delimiter ClosingSquareBracket)::rest, Extension  ->
      loop rest [] (inc_bracket_depth bracket_depth) Version
    | (Delimiter (ClosingCurlyBracket | ClosingSquareBracket))::rest, _ ->
      loop rest [] (inc_bracket_depth bracket_depth) track_block
    | (Delimiter (OpeningCurlyBracket | OpeningSquareBracket))::rest, Version ->
      let new_depth = dec_bracket_depth bracket_depth in
      if new_depth = 0 then begin
        Hashtbl.replace track_table "version" cur_info;
        loop rest [] new_depth Title
      end
      else if new_depth = 1 then begin
        Hashtbl.add track_table "version_plus" cur_info;
        loop rest [] new_depth Version
      end
      else loop rest ("["::word_acc) 0 Version
    | (Delimiter (OpeningCurlyBracket | OpeningSquareBracket))::rest, Title ->
      let new_depth = dec_bracket_depth bracket_depth in
      if new_depth = 0 then begin
        Hashtbl.add track_table "title_plus" cur_info;
        loop rest [] new_depth Title
      end
      else begin
        loop rest ("["::word_acc) 0 Version
      end
    | (Word w)::rest, _ -> loop rest (w::word_acc) bracket_depth track_block
    | (Delimiter _)::rest, _ -> loop rest word_acc bracket_depth track_block
  in
  let reserved_tokens = List.rev tokens in
  loop reserved_tokens [] 0 Extension;
  CCHashtbl.to_list track_table

let token_list_to_string list =
  let token_to_string = Track_Tokens.token_to_string in
  let string_list = List.map token_to_string list in
  String.concat ";" string_list
