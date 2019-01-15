
(* TODO *)

let test_suites : unit Alcotest.test list =
  [("Track_parser.Parser", Test_parser.Parser.tests)]

let () = Alcotest.run "Track Utils" test_suites
