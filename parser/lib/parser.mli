module Track_Tokens :
  sig
    type path_delimiter = Slash | Backshlash
    type delimiter =
        Slash
      | Backslash
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
    val cons_non_empty_buffer_as_word :
      Buffer.t -> track_token list -> track_token list
    val buffer_prev_element : Buffer.t -> int -> char
    val buffer_next_element : Buffer.t -> int -> char
    val track_tokenizer : Buffer.t -> track_token list list
  end
module Track :
  sig
    type extensions = WAV | MP3 | MP4
    type t = string
    val extension_to_string : extensions -> string
    val year : string
    val author : string
    val author_plus : string list
    val title : string
    val title_plus : string list
    val version : string
    val version_plus : string list
    val format : string
    val extension : extensions
    val compare : 'a -> 'a -> int
  end
module Track_Map :
  sig
    type key = Track.t
    type 'a t = 'a Map.Make(Track).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
val reduce_string_list : string list -> string
val parse_track_tokens : Track_Tokens.track_token list -> string Track_Map.t
