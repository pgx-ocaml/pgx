let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = env#net in
  let (module Pgx_eio_impl) = Pgx_eio.make ~net ~sw in
  let module Alcotest_io = struct
    type 'a test_case = 'a Alcotest.test_case
    let test_case name speed f = Alcotest.test_case name speed f
    let run name tests = Alcotest.run name tests
  end in
  let module Pgx_test_eio = Pgx_test.Make_tests (Pgx_eio_impl) (Alcotest_io) in
  Pgx_test_eio.run_tests ~library_name:"pgx_eio"
