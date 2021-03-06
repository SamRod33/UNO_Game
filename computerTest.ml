open OUnit2
open Card
open Player
open State
open Computer
open Yojson.Basic.Util

(** Test suite for the Computer module *)

(** [action_test name g c] is true when the card output by the computer
    action in game [g] is the same as the card [c], false otherwise. *)
let action_test name g c =
  name >:: fun _ -> assert_equal c (snd (action g))

(** [action_test name g c] is true when the card output by the computer
    action in game [g] is the same as a draw 4 card, false otherwise. *)
let action_draw4_test name g =
  name >:: fun _ ->
  let dp c = match snd c with None -> -1 | Some c -> draw_penalty c in
  assert_equal 4 (dp (action g))

(** [action_test name g c] is true when the card output by the computer
    action in game [g] is the same as a swap card [c], false otherwise. *)
let action_swap_test name g c =
  name >:: fun _ ->
  let swap_bool card =
    match card with
    | None -> false
    | Some c -> (
        match actions c with
        | { skip = _; reverse = _; swap = true, id; change_color = _ }
          ->
            (* print_string (string_of_int id); *) true
        | _ -> false)
  in
  assert_equal (swap_bool c) (swap_bool (snd (action g)))

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

let blue_draw2 =
  List.filter
    (fun x ->
      color x = B
      && digit x = None
      && actions x
         = {
             skip = false;
             reverse = false;
             swap = (false, -1);
             change_color = false;
           }
      && draw_penalty x = 2)
    std_deck
  |> List.hd

let red_draw2 =
  List.filter
    (fun x ->
      color x = R
      && digit x = None
      && actions x
         = {
             skip = false;
             reverse = false;
             swap = (false, -1);
             change_color = false;
           }
      && draw_penalty x = 2)
    std_deck
  |> List.hd

let draw4 =
  List.filter (fun x -> draw_penalty x = 4) std_deck |> List.hd

let swap =
  List.filter
    (fun x ->
      actions x
      = {
          skip = false;
          reverse = false;
          swap = (true, -1);
          change_color = true;
        })
    std_deck
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
  create_test "p1" [ red1; red2; red3; red4; red5; red6; red7 ] false 1

let p2 =
  create_test "p2" [ red1; red2; red3; red4; red5; red6; red7 ] false 1

let p3 =
  create_test "p3"
    [ blue1; blue2; blue3; blue4; blue5; blue6; blue7 ]
    false 3

let p4 =
  create_test "p4"
    [ blue1; blue2; blue3; blue4; blue5; blue6; blue7 ]
    false 4

let p5 =
  create_test "p5"
    [ blue_draw2; blue2; blue3; blue4; blue5; blue6; blue7 ]
    false 5

let p6 =
  create_test "p6"
    [ red_draw2; blue2; blue3; blue_draw2; blue5; blue6; blue7 ]
    false 6

let p7 =
  create_test "p7"
    [ draw4; blue2; blue3; blue_draw2; blue5; blue6; blue7 ]
    false 7

let p8 = create_test "p8" [ draw4 ] false 8

let p9 =
  create_test "p9"
    [ draw4; blue2; blue3; red_draw2; blue5; blue6; blue7 ]
    false 9

let p10 = create_test "p10" [ swap; red5 ] false 10

(*** states ***)
let start_red0 = t_test no_17br_deck std_deck 0 red0 [ p1; p2; p3; p4 ]

let start_blue0 =
  t_test no_17br_deck std_deck 0 blue0 [ p1; p2; p3; p4 ]

let start_blue0_2 =
  t_test no_17br_deck std_deck 2 blue_draw2 [ p5; p2; p3; p4 ]

let start_blue0_2_p6 =
  t_test no_17br_deck std_deck 2 blue_draw2 [ p6; p2; p3; p4 ]

let start_blue0_2_p7 =
  t_test no_17br_deck std_deck 2 blue_draw2 [ p7; p2; p3; p4 ]

let nextp_uno = t_test no_17br_deck std_deck 0 blue6 [ p7; p8; p3; p4 ]

let nextp_uno_43 =
  t_test no_17br_deck std_deck 0 blue6 [ p7; p8; p4; p3 ]

let nextp_uno4 = t_test no_17br_deck std_deck 0 blue6 [ p9; p8; p3; p4 ]

let nextp_uno_swap =
  t_test no_17br_deck std_deck 0 blue6 [ p10; p8; p3; p4 ]

let both_std_deck =
  t_test std_deck std_deck 2 blue_draw2 [ p6; p2; p3; p4 ]

let both_nobr_deck =
  t_test no_17br_deck no_17br_deck 2 blue_draw2 [ p6; p2; p3; p4 ]

(**************************************************************)

let computer_suite =
  [
    action_test "no_17br red0 -> red1" start_red0 (Some red1);
    action_test "no_17br blue0 -> None" start_blue0 None;
    action_test "std blue0 2 -> blue_draw2" start_blue0_2
      (Some blue_draw2);
    action_test "std blue0 2 p6 -> red_draw2" start_blue0_2_p6
      (Some red_draw2);
    action_test "std blue0 2 p7 -> blue_draw2" start_blue0_2_p7
      (Some blue_draw2);
    action_test "nextp -> blue_draw2" nextp_uno (Some blue_draw2);
    action_test "nextp_uno_43 -> blue_draw2" nextp_uno_43
      (Some blue_draw2);
    action_draw4_test "nextp_4 -> draw4" nextp_uno4;
    action_swap_test "swap -> swap" nextp_uno_swap (Some swap);
    action_test "std blue0 2 p6 -> red_draw2, both std" both_std_deck
      (Some red_draw2);
    action_test "std blue0 2 p6 -> red_draw2, both nobr" both_nobr_deck
      (Some red_draw2);
  ]

let suite = "test suite for State" >::: computer_suite

(* let _ = run_test_tt_main suite *)
