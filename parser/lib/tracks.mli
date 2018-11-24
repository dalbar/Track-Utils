module Track_Tokens :
  sig
    type path_delimiter = Slash | Backshlash
    type delimiter =
        Slash
      | OpeningCurlyBracket
      | OpeningSquareBracket
      | ClosingCurlyBracket
      | ClosingSquareBracket
      | AuthorTitleDelimiter
    val delimiter_to_string : delimiter -> string
    type prefix = Vinyl
    val prefix_to_string : prefix -> string
    type feature_operator = Feature | Ft | And
    val feature_operator_to_string : feature_operator -> string
    type operator = Dot | Feature of feature_operator
    val operator_to_string : operator -> string
    type track_token =
        Operator of operator
      | Word of string
      | Prefix of prefix
      | Delimiter of delimiter
    val flush_buffer : Buffer.t -> string
    val cons_non_empty_buffer_as_word :
      Buffer.t -> track_token list -> track_token list
    val buffer_prev_element : Buffer.t -> int -> char
    val buffer_next_element : Buffer.t -> int -> char
    val token_to_string : track_token -> string
    val track_tokenizer : Buffer.t -> track_token list list
    val track_tokenizer_string : string -> track_token list list
  end
module Track_Table :
  sig
    type extensions = WAV | MP3 | MP4
    val inital_key_values : (string * string) list
    val empty : unit -> (string, string) Hashtbl.t
  end
type track_record = {
  year : string;
  vinyl : bool;
  author : string;
  feature_operator : string;
  features : string;
  title : string;
  title_plus : string list;
  version : string;
  version_plus : string list;
  extension : string;
}
type track_block = Year | AuthorAndPrefix | Title | Version | Extension
val empty : track_record
val year_regexp : Re__.Core.re
val parse_track_tokens_to_hashtbl :
  Track_Tokens.track_token list -> (string, string) Hashtbl.t
val token_hashtbl_to_token_record :
  (string, string) Hashtbl.t -> track_record
val parse_track_tokens : Track_Tokens.track_token list -> track_record
val parse_track_tokens_list :
  Track_Tokens.track_token list list -> track_record list
val token_list_to_string : Track_Tokens.track_token list -> string
val stringify_token_record : track_record -> string
val parse_string_list : string list -> track_record list
