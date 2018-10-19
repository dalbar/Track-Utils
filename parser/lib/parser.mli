type track_token =
    OpeningBracketDelimiter of char
  | ClosingBracketDelimiter of char
  | PathDelimiter of char
  | Operator of char
  | Word of string
  | Prefix of string
val flush_buffer : Buffer.t -> string
val cons_non_empty_buffer_as_word :
  Buffer.t -> track_token list -> track_token list
val buffer_prev_element : Buffer.t -> int -> char
val buffer_next_element : Buffer.t -> int -> char
val track_tokenizer : Buffer.t -> track_token list list
