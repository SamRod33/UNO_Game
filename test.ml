include Card_test
include Player_test
include Computer_test
include State_test
open OUnit2

(** [test_suites] is the list combining all tests in the program. *)
let test_suites =
  List.flatten
    [
      Card_test.card_suite;
      Player_test.player_suite;
      Computer_test.computer_suite;
      State_test.state_suite;
    ]

let full_suite = "Testing entire suite" >::: test_suites

let _ = run_test_tt_main full_suite
