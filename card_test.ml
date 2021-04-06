open OUnit2
open Card

(** [f_test name f c expected to_str] creates an OUnit test case called
    [name] that checks [expected] equals [f c]. [to_str] dictates how
    the test case should print out the actual value of [f c] if
    otherwise. *)
let f_test name f c expected to_str =
  name >:: fun _ -> assert_equal expected (f c) ~printer:to_str

(** [penalty_test name c expected] creates an OUnit test case that
    checks [expected] equals [draw_penalty c]. *)
let penalty_test name c expected =
  f_test name draw_penalty c expected string_of_int

(** [string_of_color] is the string representation of color from Card.t. *)
let string_of_color = function
  | R -> "red"
  | G -> "green"
  | B -> "blue"
  | Y -> "yellow"
  | ANY -> "any"

let string_of_actions c =
  "skip: " ^ string_of_bool c.skip ^ " reverse: "
  ^ string_of_bool c.reverse
  ^ "swap: " ^ string_of_bool c.swap ^ "change color: "
  ^ string_of_bool c.change_color

(** [color_test name c expected] creates an OUnit test case that checks
    [expected] equals [color c]. *)
let color_test name c expected =
  f_test name color c expected string_of_color

(** [actions_test name c expected] creates an OUnit test case that
    checks [expected] equals [actions c]. *)
let actions_test name c expected =
  f_test name actions c expected string_of_actions

(** [amt_test name c expected] creates an OUnit test case that checks
    [expected] equals [amount c]. *)
let amt_test name c expected =
  f_test name amount c expected string_of_int

(** [create_cards lst] is the parsed list of JSONs containing Cards. *)
let rec create_cards lst = List.map create lst

(** [deck] is the list of cards from [custom_card.json] used in testing. *)
let deck =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  "custom_card.json" |> from_file |> member "standard deck" |> to_list
  |> create_cards

(** [colors] is the expected colors for all cards in order. *)
let colors = [ R; G; ANY; ANY; B ]

(** [penalties] is the expected penalties for all cards in order. *)
let penalties = [ 2; 0; 0; 4; 0 ]

(** [amounts] is the expected amounts for all cards in order. *)
let amounts = [ 1; 2; 4; 1; 3 ]

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
let num_action = make_action false false false false

(** [plus_4_action] is the action of a +4 card. *)
let plus_4_action = make_action true false false true

let penalty_tests = f_tests penalty_test "penalty tests" deck penalties

let color_tests = f_tests color_test "color tests" deck colors

let actions_tests =
  [
    actions_test "number card action is all false" (List.hd deck)
      num_action;
    actions_test "+4 card action is skip T, change color T, rest F"
      (List.nth deck 3) plus_4_action;
  ]

let amt_tests = f_tests amt_test "amount tests" deck amounts

let suite =
  "test suite for Card"
  >::: List.flatten
         [ penalty_tests; color_tests; actions_tests; amt_tests ]

let _ = run_test_tt_main suite
