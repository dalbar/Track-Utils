open Track_utils_parser.Parser

let wav_no_version = "1999/Sample Artist - Sample Track.wav\n"
let mp4_mixed = "1m4a/Yann Tiersen - Comptine D'Un Autre Été, L'Après-Midi (Live).2008.m4a"

let sample_use () =
  let test_buffer = Buffer.create 180 in
  Buffer.add_string test_buffer wav_no_version;
  Buffer.add_string test_buffer mp4_mixed;
  print_string "test wav and mp3, mp4\n";
  let tokens = Track_Tokens.track_tokenizer test_buffer in
  tokens
let test_tokenizer () =
  Alcotest.(check int) "a" 1 1
let plus () = Alcotest.(check int) "same ints" 7 7
let tests = [
    "Test my A", `Quick, plus;
]
