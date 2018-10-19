  let plus () = Alcotest.(check int) "same ints" 7 7
  let tests = [
    "Test my A", `Quick, plus;
]