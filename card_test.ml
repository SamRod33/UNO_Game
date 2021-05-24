open OUnit2
open Card
open Test

(** [penalty_test name c expected] creates an OUnit test case that
    checks [expected] equals [draw_penalty c]. *)
let penalty_test name c expected =
  f_test name draw_penalty c expected string_of_int

(** [color_test name c expected] creates an OUnit test case that checks
    [expected] equals [color c]. *)
let color_test name c expected =
  f_test name color c expected string_of_color

(** [img_test name c expected] creates an OUnit test case that checks
    [expected] equals [img c]. *)
let img_test name c expected = f_test name img c expected Fun.id

(** [actions_test name c expected] creates an OUnit test case that
    checks [expected] equals [actions c]. *)
let actions_test name c expected =
  f_test name actions c expected string_of_actions

(** [amt_test name c expected] creates an OUnit test case that checks
    [expected] equals [amount c]. *)
(* let amt_test name c expected = f_test name amount c expected
   string_of_int *)

(** [colors] is the expected colors for all cards in order. *)
let colors = [ R; G; ANY; ANY; B ]

(** [penalties] is the expected penalties for all cards in order. *)
let penalties = [ 2; 0; 0; 4; 0 ]

(** [f_tests f name cards expecteds] creates many OUnit test cases that
    check [f name c expected], where [c] is a card in [cards], equals
    [expected], where [expected] is an expected value in [expecteds].
    Assumes [cards] is in the same order as [expecteds]. *)
let rec f_tests f name cards expecteds =
  match expecteds with
  | [] -> []
  | expected :: t2 -> (
      match cards with
      | [] -> failwith "No more cards"
      | h :: t1 -> f name h expected :: f_tests f name t1 t2)

(** [make_action skip reverse swap change_color] is a created action of
    a Card*)
let make_action skip reverse swap change_color =
  { skip; reverse; swap; change_color }

(** [num_action] is the action of a number card. *)
let num_action = make_action false false (false, -1) false

(** [plus_4_action] is the action of a +4 card. *)
let plus_4_action = make_action true false (false, -1) true

let penalty_tests =
  f_tests penalty_test "penalty tests" custom_cards penalties

let color_tests = f_tests color_test "color tests" custom_cards colors

let actions_tests =
  [
    actions_test "number card action is all false"
      (List.hd custom_cards) num_action;
    actions_test "+4 card action is skip T, change color T, rest F"
      (List.nth custom_cards 3)
      plus_4_action;
  ]

let img_tests =
  [
    img_test "Red +2 img is Red-draw2.png" (List.hd custom_cards)
      "Red-draw2";
    img_test "Wild card img is Wild.png"
      (List.nth custom_cards 2)
      "Wild";
    img_test "Blue skip img Blue-skip.png"
      (List.nth custom_cards 4)
      "Blue-skip";
  ]

let card_suite =
  List.flatten [ penalty_tests; color_tests; actions_tests; img_tests ]

let suite = "test suite for Card" >::: card_suite

(* let _ = run_test_tt_main suite *)
