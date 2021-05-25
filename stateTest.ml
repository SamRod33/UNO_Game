open OUnit2
open Card
open Player
open State
open ComputerTest

(** Test suite for the State module *)

(** [current_player_test name g player] creates an OUnit test case that
    checks if the current player id from game [g] matches the id of the
    player [player]. *)
let current_player_test name g player =
  name >:: fun _ -> assert_equal (id (current_player g)) (id player)

let deck = standard_cards

let p1 = create "p1" false 1

let p2 = create "p2" false 2

let p3 = create "p3" false 3

let p4 = create "p4" false 4

let player_list = [ p1; p2; p3; p4 ]

let player_list2 = [ p3; p4; p1; p2 ]

let random_state = init_state deck player_list

let p3_start_state = init_state deck player_list2

let starting_player = current_player random_state

let state = t_test deck deck 0

type test_result =
  | Pass
  | Fail

(** [top_card_test name c g] creates an OUnit test case that checks if
    the top card from game [g] matches the card [card]. *)
let top_card_test name c g =
  name >:: fun _ -> assert_equal c (top_card g)

(** [stack_penalty_test name i g] creates an OUnit test case that checks
    if the stack penalty from game [g] matches the integer [i]. *)
let stack_penalty_test name i g =
  name >:: fun _ ->
  assert_equal i (stack_penalty g) ~printer:string_of_int

(** [result_of_play g] is [Pass] when the game play is valid and [Fail]
    if the game results in an illegal move. *)
let result_of_play g = match g with Illegal -> Fail | _ -> Pass

(** [play_test name c g r] creates an OUnit test case that checks if the
    result of the play from game [g] matches the result [r]. *)
let play_test name c g r =
  name >:: fun _ -> assert_equal (result_of_play (play (Some c) g)) r

(** [next_game_state g] creates an OUnit test case that checks if the
    next gamestate of game [g] is [Legal]. *)
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

(* let _ = run_test_tt_main suite *)
