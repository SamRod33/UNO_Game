open OUnit2
open Card
open Player
open State
open Computer_test

(** Test suite for the State module *)

let current_player_test name g player =
  name >:: fun _ -> assert_equal (id (current_player g)) (id player)

let deck = standard_cards

let p1 = create "p1" false

let p2 = create "p2" false

let p3 = create "p3" false

let p4 = create "p4" false

let player_list = [ p1; p2; p3; p4 ]

let player_list2 = [ p3; p4; p1; p2 ]

let random_state = init_state deck player_list

let p3_start_state = init_state deck player_list2

let starting_player = current_player random_state

let state = t_test deck deck 0

type test_result =
  | Pass
  | Fail

let top_card_test name c g =
  name >:: fun _ -> assert_equal c (top_card g)

let stack_penalty_test name i g =
  name >:: fun _ ->
  assert_equal i (stack_penalty g) ~printer:string_of_int

let result_of_play g = match g with Illegal -> Fail | _ -> Pass

let play_test name c g r =
  name >:: fun _ -> assert_equal (result_of_play (play (Some c) g)) r

let next_game_state g =
  match g with Legal g -> g | _ -> failwith "illegal play"

let state_suite =
  [
    current_player_test "p1 first player" random_state p1;
    current_player_test "p3 first player" p3_start_state p3;
    top_card_test "blue +2 top card" blue_draw2 start_blue0_2;
    top_card_test "red 0" red0 start_red0;
    stack_penalty_test "2 stack penalty" 2 start_blue0_2;
    stack_penalty_test "0 stack penalty" 0 start_red0;
    play_test "play blue +2 on blue +2" blue_draw2 start_blue0_2 Pass;
    stack_penalty_test "play +2 on existing 2 stack penalty" 4
      (next_game_state (play (Some blue_draw2) start_blue0_2));
  ]

let suite = "test suite for State" >::: state_suite

let _ = run_test_tt_main suite
