open Core
module Value = Pgx_value_core

let time_roundtrip str = Value.of_string str |> Value.to_time_exn
let printer = Time_float.to_string_abs ~zone:Time_float.Zone.utc

let time_testable =
  Alcotest.testable (fun ppf t -> Format.pp_print_string ppf (printer t)) Time_float.equal
;;

let check_time = Alcotest.check time_testable
let check_string = Alcotest.(check string)

let test_time_of_string _ =
  let expected = Time_float.of_string_with_utc_offset "2016-03-15 19:55:18.123456-04:00" in
  check_time "without TZ" expected (time_roundtrip "2016-03-15 23:55:18.123456");
  check_time "zulu" expected (time_roundtrip "2016-03-15 23:55:18.123456Z");
  check_time "hour TZ" expected (time_roundtrip "2016-03-15 19:55:18.123456-04");
  check_time "full TZ" expected (time_roundtrip "2016-03-15 19:55:18.123456-04:00")
;;

let test_time_of_string_no_ms _ =
  let expected = Time_float.of_string_with_utc_offset "2016-03-15 19:55:18-04:00" in
  check_time "without TZ" expected (time_roundtrip "2016-03-15 23:55:18");
  check_time "zulu" expected (time_roundtrip "2016-03-15 23:55:18Z");
  check_time "hour TZ" expected (time_roundtrip "2016-03-15 19:55:18-04");
  check_time "full TZ" expected (time_roundtrip "2016-03-15 19:55:18-04:00")
;;

let test_time_conversion_roundtrip _ =
  let expected_str = "2016-03-15 23:55:18.123456Z" in
  check_string "parse-print" expected_str (time_roundtrip expected_str |> printer);
  let expected_time = Time_float.of_string_with_utc_offset expected_str in
  check_time "print-parse" expected_time (Value.of_time expected_time |> Value.to_time_exn)
;;

let time_tests =
  [ Alcotest.test_case "test time_of_string" `Quick test_time_of_string
  ; Alcotest.test_case
      "test time_of_string no milliseconds"
      `Quick
      test_time_of_string_no_ms
  ; Alcotest.test_case
      "test time conversion roundtrip"
      `Quick
      test_time_conversion_roundtrip
  ]
;;

let () = Alcotest.run "pgx_async_conversions" [ "time", time_tests ]
