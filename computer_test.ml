open OUnit2
open Card
open Player
open State
open Computer
open Yojson.Basic.Util

(** Test suite for the Computer module *)

let basic_action_test name g c =
  name >:: fun _ -> assert_equal c (basic_action g)

let action_test name g c = name >:: fun _ -> assert_equal c (action g)

(*********************** create game g *************************)

let std_deck = standard_cards

(*** cards ***)
let red0 =
  List.filter
    (fun x ->
      color x = R
      && digit x = Some 0
      && actions x
         = {
             skip = false;
             reverse = false;
             swap = (false, -1);
             change_color = false;
           }
      && draw_penalty x = 0)
    std_deck
  |> List.hd

let red1 =
  List.filter (fun x -> color x = R && digit x = Some 1) std_deck
  |> List.hd

let red2 =
  List.filter (fun x -> color x = R && digit x = Some 2) std_deck
  |> List.hd

let red3 =
  List.filter (fun x -> color x = R && digit x = Some 3) std_deck
  |> List.hd

let red4 =
  List.filter (fun x -> color x = R && digit x = Some 4) std_deck
  |> List.hd

let red5 =
  List.filter (fun x -> color x = R && digit x = Some 5) std_deck
  |> List.hd

let red6 =
  List.filter (fun x -> color x = R && digit x = Some 6) std_deck
  |> List.hd

let red7 =
  List.filter (fun x -> color x = R && digit x = Some 7) std_deck
  |> List.hd

let blue0 =
  List.filter
    (fun x ->
      color x = B
      && digit x = Some 0
      && actions x
         = {
             skip = false;
             reverse = false;
             swap = (false, -1);
             change_color = false;
           }
      && draw_penalty x = 0)
    std_deck
  |> List.hd

let blue1 =
  List.filter (fun x -> color x = B && digit x = Some 1) std_deck
  |> List.hd

let blue2 =
  List.filter (fun x -> color x = B && digit x = Some 2) std_deck
  |> List.hd

let blue3 =
  List.filter (fun x -> color x = B && digit x = Some 3) std_deck
  |> List.hd

let blue4 =
  List.filter (fun x -> color x = B && digit x = Some 4) std_deck
  |> List.hd

let blue5 =
  List.filter (fun x -> color x = B && digit x = Some 5) std_deck
  |> List.hd

let blue6 =
  List.filter (fun x -> color x = B && digit x = Some 6) std_deck
  |> List.hd

let blue7 =
  List.filter (fun x -> color x = B && digit x = Some 7) std_deck
  |> List.hd

(*** deck ***)

let no_17br_deck =
  List.filter
    (fun x ->
      x = red1 || x = red2 || x = red3 || x = red4 || x = red5
      || x = red6 || x = red7 || x = blue1 || x = blue2 || x = blue3
      || x = blue4 || x = blue5 || x = blue6 || x = blue7)
    std_deck

(*** players ***)
let p1 =
  create_test "p1" [ red1; red2; red3; red4; red5; red6; red7 ] false

let p2 =
  create_test "p2" [ red1; red2; red3; red4; red5; red6; red7 ] false

let p3 =
  create_test "p3"
    [ blue1; blue2; blue3; blue4; blue5; blue6; blue7 ]
    false

let p4 =
  create_test "p4"
    [ blue1; blue2; blue3; blue4; blue5; blue6; blue7 ]
    false

(*** states ***)
let start_red0 = t_test no_17br_deck std_deck 0 red0 [ p1; p2; p3; p4 ]

let start_blue0 =
  t_test no_17br_deck std_deck 0 blue0 [ p1; p2; p3; p4 ]

(**************************************************************)

let suite =
  "test suite for State"
  >::: [
         basic_action_test "basic no_17br red0 -> red1" start_red0
           (Some red1);
         basic_action_test "basic no_17br blue0 -> None" start_blue0
           None;
         action_test "no_17br red0 -> red1" start_red0 (Some red1);
         action_test "no_17br blue0 -> None" start_blue0 None;
       ]

let _ = run_test_tt_main suite
