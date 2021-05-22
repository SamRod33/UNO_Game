open Graphics
open Constants
open Window_gui

(*example_for_testing********************************************************************************************************)

open Card
open Computer
open State
open Player
open Yojson.Basic.Util

let std_deck = standard_cards

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

let p1 = create_test "p1" [ swap; blue1 ] false

let p2 = create_test "p2" [ blue1; blue2 ] false

let p3 = create_test "p3" [ blue1; blue2; blue3 ] false

let p4 = create_test "p4" [ blue1; blue2; blue3; blue4 ] false

let p5 = create_test "p5" [ blue1; blue2; blue3; blue4; blue5 ] false

let p6 =
  create_test "p6" [ blue1; blue2; blue3; blue4; blue5; blue6 ] false

let swap_window_test =
  t_test std_deck std_deck 0 blue6 [ p1; p2; p3; p4; p5; p6 ]

(****************************************************************************************************************************)

;;
open_window;
draw_swap_player_screen swap_window_test;
highlight_selection _GOLD _BLACK 0 !outline_pswap_x !outline_pswap_y
  outline_swap_width outline_swap_height;

try
  while running do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _CONFIRM then
      failwith
        ("TODO: return player_selected: "
        ^ string_of_int !card_selected_idx)
    else if st.key = _DOWN_KEY then
      if
        !outline_pos_y
        <= swap_start_pos_y - ((*num_players g*) 5 * fst swap_space)
      then ()
      else (
        highlight_selection _GOLD _BLACK (fst card_space) !outline_pos_x
          !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + fst card_space;
        card_selected_idx := !card_selected_idx + 1)
    else if st.key = _LEFT_KEY then
      if !outline_pos_x <= cards_start_pos_x - 10 then ()
      else (
        highlight_selection _GOLD _BLACK
          ~-(fst card_space)
          !outline_pos_x !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + ~-(fst card_space);
        card_selected_idx := !card_selected_idx - 1)
  done
with Exit -> ()
