type track_token =
  | OpeningBracketDelimiter of char
  | ClosingBracketDelimiter of char
  | PathDelimiter of char
  | Operator of char
  | Word of string
  | Prefix of string

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
  let lexer_buf = Buffer.create 180 in
  let rec loop tracks tokens curPosition =
    try
      let cur_char = Buffer.nth track_buffer curPosition in
      match cur_char with
      | 'v' -> if buffer_next_element track_buffer curPosition = '_' then begin
          loop tracks (Prefix "v"::tokens) (curPosition+2)
        end
        else begin
          Buffer.add_char lexer_buf cur_char; loop tracks tokens (curPosition + 1)
        end
      | '(' | '[' ->
        let tmp_tokens = cons_non_empty_buffer_as_word lexer_buf tokens in
        loop tracks (OpeningBracketDelimiter cur_char::tmp_tokens) (curPosition + 1)
      | ')' | ']' ->
        let tmp_tokens = cons_non_empty_buffer_as_word lexer_buf tokens in
        loop tracks (ClosingBracketDelimiter cur_char::tmp_tokens) (curPosition + 1)
      | '/' ->
        let tmp_tokens = cons_non_empty_buffer_as_word lexer_buf tokens in
        loop tracks (PathDelimiter cur_char::tmp_tokens) (curPosition + 1)
      | ' ' ->
        let tmp_tokens = cons_non_empty_buffer_as_word lexer_buf tokens in
        loop tracks tmp_tokens (curPosition+1)
      | '-' ->
        if buffer_prev_element track_buffer curPosition = ' '
        && buffer_next_element track_buffer curPosition = ' ' then begin
            let tmp_tokens = cons_non_empty_buffer_as_word lexer_buf tokens in
            loop tracks (Operator cur_char::tmp_tokens) (curPosition + 2)
        end
        else begin
          Buffer.add_char lexer_buf cur_char; loop tracks tokens (curPosition + 1)
        end
      | '\n' -> Buffer.clear lexer_buf; loop ((List.rev tokens)::tracks) [] (curPosition+1)
      | _ -> Buffer.add_char lexer_buf cur_char; loop tracks tokens (curPosition + 1)
    with Invalid_argument _ -> (List.rev tokens)::tracks
  in loop [] [] 0
