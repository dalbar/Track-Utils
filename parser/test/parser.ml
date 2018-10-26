open Track_utils_parser.Parser

let wav_no_version = "1999/Sample Artist - Sample Track.wav"
let wav_token_string = "1999;/;Sample;Artist;-;Sample;Track;.;wav"

let mp4_mixed_frensh = "1m4a/Yann Tiersen - Comptine D'Un Autre Été, L'Après-Midi (Live).2008.m4a"

let sample_use () =
  let test_buffer = Buffer.create 180 in
  Buffer.add_string test_buffer wav_no_version;
  print_string "test wav and mp3, mp4\n";
  let tokens = Track_Tokens.track_tokenizer test_buffer in
  tokens

let test_tokenizer_simple_wav_token () =
let test_buffer = Buffer.create 180 in
  Buffer.add_string test_buffer wav_no_version;
  let tokens = Track_Tokens.track_tokenizer test_buffer in
  let token_string = token_list_to_string (List.nth tokens 0) in
  print_string (Buffer.contents test_buffer);
  Alcotest.(check string) "Simple Wav without version" wav_token_string token_string

let tests = [
    "Test tokenizer", `Quick, test_tokenizer_simple_wav_token;
]
