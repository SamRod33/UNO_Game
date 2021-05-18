open OUnit2
open Card
open Player
open State

(** Test suite for the State module *)

let current_player_test name g player =
  name >:: fun _ -> assert_equal (id (current_player g)) (id player)

let deck = standard_cards

let p1 = create "p1" false

let p2 = create "p2" false

let p3 = create "p3" false

let p4 = create "p4" false

let start_state = init_state deck [ p1; p2; p3; p4 ]

let p3_start_state = init_state deck [ p3; p4; p1; p2 ]

let starting_player = current_player start_state

let state_suite =
  [
    current_player_test "p1 first player" start_state p1;
    current_player_test "p3 first player" p3_start_state p3;
  ]

let suite = "test suite for State" >::: state_suite

let _ = run_test_tt_main suite
