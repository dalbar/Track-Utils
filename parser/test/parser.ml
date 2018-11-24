open Track_utils_parser.Tracks

let wav_no_version = "1999/Sample Artist - Sample Track.wav"
let wav_token_string = "1999;/;Sample;Artist;-;Sample;Track;.;wav"

let wav_mixed = "1992/v_Chez Damier feature Alu & Mic - Can You Feel It (more info (a b c)) (A 2 1:2 Step (version plus 1) (version plus 2 (useless add))).wav"

let mp3_1 = "1mp3/Black - Wonderful Life (Caval & Razaar Edit 2011).2011.mp3"
let mp3_2 = "1mp3/Black - Wonderful Life (Kenny Ground & Dave Martins Bootleg).2011.mp3"
let mp3_3 = "1mp3/Black & Marty ft random - Wonderful Life (more info for title) (Nick Tohme Bootleg (Special Version) (Mix)).2011.mp3"
let mp3_4 = "1mp3/Jamie Woon - Night Air (Official Video).2010.mp3"

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
