open OUnit2
open Player
open TestHelper

(** [remove_raises name p c] creates an OUnit test case that checks if
    [CardNotInHand c] is raised when [p] does not contain [c]. *)
let remove_raise_test name p c =
  name >:: fun _ ->
  assert_raises (CardNotInHand c) (fun () -> remove_card p c)

(** [player_hand_test name lst exp] creates an OUnit test case that
    checks if the card list [exp] returned by the player hand function
    is the same as the given card list [lst]. *)
let player_hand_test name lst exp =
  name >:: fun _ ->
  assert_equal exp (player_hand lst) ~printer:(pp_list pp_card)
    ~cmp:cmp_set_like_lists

(** [string_of_t x] is the string representation of the abstract type t. *)
let string_of_t x = "Abstract <T>"

(** [fst_card] is the first card in the standard deck*)
let fst_card = List.hd Card.standard_cards

(** [snd_card] is the second card in the standard deck*)
let snd_card =
  match Card.standard_cards with
  | h :: b :: t -> b
  | _ -> failwith "Impossible"

let empty : Player.t = create "Charles" false 1

let uno = add_card empty fst_card

let uno_kanye = add_card (create "Kanye" false 2) fst_card

let two_cards = add_card uno snd_card

let name_tests =
  [
    f_test "name of empty is Charles" name empty "Charles" (fun s -> s);
    f_test "name of uno_kanye is Kanye" name uno_kanye "Kanye" (fun s ->
        s);
    f_test "name of two_cards is Charles" name two_cards "Charles"
      (fun s -> s);
  ]

let player_hand_tests =
  [
    player_hand_test "empty hand is []" empty [];
    player_hand_test
      "uno hand is [(red, 0, 0, skip: false reverse: false swap: false \
       change color: false)]"
      uno [ fst_card ];
    player_hand_test
      "two_cards hand is [(red, 0, 0, skip: false reverse: false swap: \
       false change color: false), (red, 1, 0, skip: false reverse: \
       false swap: false change color: false)]"
      two_cards [ fst_card; snd_card ];
  ]

let is_uno_tests =
  [
    f_test "empty hand is uno" is_uno empty true string_of_bool;
    f_test "uno hand is uno" is_uno uno true string_of_bool;
    f_test "two_cards hand is not uno" is_uno two_cards false
      string_of_bool;
  ]

let remove_card_tests =
  [
    f2_test "remove snd card from two_cards is uno" remove_card
      two_cards snd_card uno string_of_t;
    f2_test "remove fst card from uno is empty" remove_card uno fst_card
      empty string_of_t;
    remove_raise_test "fst card is not in empty, raise CardNotInHand"
      empty fst_card;
  ]

let player_suite =
  List.flatten
    [ name_tests; player_hand_tests; is_uno_tests; remove_card_tests ]

let suite = "test suite for Player" >::: player_suite

(* let _ = run_test_tt_main suite *)
