open Graphics
open Constants
open WindowGui
open State
open Player

(* [outline_width, outline_height] is the dimensions of the outline. *)
let outline_width, outline_height = (160, 210)

(* [swap_txt_pos_x, swap_txt_pos_y] is the position of the swap player
   prompt, as inspired by the mockup. *)
let swap_txt_pos_x, swap_txt_pos_y = (logo_pos_x - 150, logo_pos_y - 100)

(* [swap_start_pos_x, swap_start_pos_y] is the starting position of the
   swap player info cards, as inspired by the mockup. *)
let swap_start_pos_x, swap_start_pos_y = (172, 224)

(* [outline_pos_x, outline_pos_y] is the starting position of the
   selection outline. *)
let outline_pos_x, outline_pos_y =
  (ref (swap_start_pos_x - 10), ref (swap_start_pos_y - 60))

(* [num_start_x, num_start_y] is the starting position of the number of
   cards each opponent has. *)
let num_offset_x, num_offset_y =
  (swap_start_pos_x - 65, swap_start_pos_y - 60)

(* [num_space] is the spacing between the number of cards in each hand. *)
let num_space = 12

(* [player_selected_idx] is the index of the player selected. *)
let player_selected_idx = ref 0

(* [swap_player_cards g] is the number of swap cards shown. *)
let swap_player_cards g = List.length (players g) - 1

(* [stid_handl_list g] is the list of player ids and their corresponding
   number of cards in game [g]. *)
let stid_handl_list g =
  let cp = current_player g in
  let other_player =
    (List.filter (fun p -> id p <> id cp)) (players g)
  in
  List.map (fun p -> (id p, List.length (player_hand p))) other_player

(* [display_hand_amt i (x, y)] is the number [i] drawn using GUI images
   at position [(x, y)]. *)
let rec display_hand_amt i (x, y) =
  if i < 10 then upload_img _TEXT_DIR (string_of_int i ^ "_med") x y
  else if i < 100 then (
    let fst_dig = i / 10 in
    let snd_dig = i mod 10 in
    upload_img _TEXT_DIR (string_of_int fst_dig ^ "_med") x y;
    display_hand_amt snd_dig (x + num_space, y))

(* [draw_player_hand_card c (x, y)] draws the card [c] for the player's
   hand in the window starting at position [(x, y)]. *)
let draw_player_hand_card c (x, y) =
  let id, amt = c in
  upload_img _ASSET_DIR ("player_hand_" ^ string_of_int id) x y;
  display_hand_amt amt (x + num_offset_x, y + num_offset_y)

(** [draw_cards g (x,y)] draws for the opponent players in game [g]
    starting at [(x,y)]. Ensures spacing between each card. *)
let rec draw_cards g (x, y) =
  let rec drawing l (x, y) =
    let p_space_x, p_space_y = swap_space in
    match l with
    | [] -> ()
    | h :: t ->
        draw_player_hand_card h (x, y);
        drawing t (x + p_space_x, y + p_space_y)
  in
  drawing (stid_handl_list g) (x, y)

(* [draw_swap_player_screen g] is the swap player screen with the card
   and players displayed from game [g]. *)
let draw_swap_player_screen g =
  open_window;
  set_background _BLACK;
  draw_logo ();
  upload_img _TEXT_DIR "Choose a player" swap_txt_pos_x swap_txt_pos_y;
  draw_cards g (cards_swap_start_pos_x, cards_swap_start_pos_y)

(* [guiid_id g] is a list of integer * integer tuples that contain the
   the gui id and player id of each player in game [g]. *)
let guiid_id g =
  let rec tup_creator ply_lst acc =
    match ply_lst with
    | [] -> acc
    | p :: t -> tup_creator t ((List.length acc, id p) :: acc)
  in
  List.rev
    (tup_creator
       (g |> players |> List.filter (fun p -> p <> current_player g))
       [])

(* [print_id_lst g] prints the gui id, player id tuples from game [g] to
   the terminal. *)
let rec print_id_lst = function
  | [] -> ()
  | (gui, id) :: t ->
      print_string ("\n" ^ string_of_int gui ^ ": " ^ string_of_int id);
      print_id_lst t

(* [get_id gui_idx g] is the player id in game [g] of the selected
   player's id [gui_idx] from the swap window. *)
let get_id gui_idx g = string_of_int (List.assoc gui_idx (guiid_id g))

(** [move op space color_a color_b] draws the new selection. *)
let move op space color_a color_b =
  highlight_selection color_a color_b (op 0 space) !outline_pos_x
    !outline_pos_y outline_width outline_height;
  outline_pos_x := op !outline_pos_x space

(* [swap_player_phase st g] launches the swap player window phase for
   game [g]. *)
let swap_player_phase st g =
  if st.key = _QUIT_KEY then exit 0
  else if st.key = _CONFIRM_KEY then raise Exit
  else if st.key = _RIGHT_KEY then
    if !outline_pos_x >= (swap_player_cards g - 1) * fst swap_space then
      ()
    else (
      move ( + ) (fst swap_space) _GOLD _BLACK;
      player_selected_idx := !player_selected_idx + 1)
  else if st.key = _LEFT_KEY then
    if !outline_pos_x <= swap_start_pos_x - 10 then ()
    else (
      move ( - ) (fst swap_space) _GOLD _BLACK;
      player_selected_idx := !player_selected_idx - 1)

(*********************************************************************)
(****** EVERYTHING UNTIL NEXT COMMENT LINE IS FOR TESTING ONLY *******)
(*********************************************************************)

open Card
open Computer
open Yojson.Basic.Util

let std_deck = standard_cards

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

let draw4 =
  List.filter (fun x -> draw_penalty x = 4) std_deck |> List.hd

let p1 =
  create_test "p1"
    [
      red1;
      red2;
      red3;
      red4;
      red5;
      red6;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
      red7;
    ]
    false 1

let p2 = create_test "p2" [ draw4 ] false 2

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
    [ draw4; blue2; blue3; blue1; blue5; blue6; blue7 ]
    false 5

let g2 = t_test std_deck std_deck 0 swap [ p1; p2; p3; p4; p5 ]

let g = t_test std_deck std_deck 0 blue6 [ p5; p2; p1; p4 ]

(******************************************************************************)

(* [swap_player_win g] is the id of the selected player to swap with in
   game [g]. *)
let swap_player_win (*()*) g =
  (* print_id_lst (guiid_id g); print_string "\nfinal id to swap hands
     with: "; *)
  draw_swap_player_screen g;
  highlight_selection _GOLD _BLACK 0 !outline_pos_x !outline_pos_y
    outline_width outline_height;
  (try
     while running do
       let st = wait_next_event [ Key_pressed ] in
       synchronize ();
       swap_player_phase st g
     done
   with Exit -> ());
  get_id !player_selected_idx g

(* ;; open_window; let chosen_player = swap_player_win () in
   print_endline chosen_player *)
