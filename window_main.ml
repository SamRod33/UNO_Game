open Graphics
open Constants
open Images
open Png
open Window_gui

let player_cards = Card.standard_cards

let outline_width, outline_height = (130, 180)

let selected_spacing = 161

let card_selected_idx = ref 0

let card_init_pos = (40, 35)

let card_start_pos = ref card_init_pos

let card_end_pos = ref (List.length player_cards * fst card_space)

let outline_pos_x = ref 30

let outline_pos_y = ref 25

let draw_top_card card = upload_img _CARD_DIR card 272 330

let draw_card_deck () = upload_img _CARD_DIR "back" 84 330

let set_player_hand_background () =
  set_color _GREEN;
  fill_rect 0 0 (int_of_string _WIDTH) 275

let draw_game_frames () =
  upload_img _ASSET_DIR "game_keys" 632 553;
  upload_img _ASSET_DIR "player_turn_frame" 84 553;
  upload_img _ASSET_DIR "penalty_frame" 448 330;
  upload_img _ASSET_DIR "player_hand_frame" 624 330

(** [move op1 op2 limit] draws the selection and if the player selection
    satisfies [limit] then the cards are shifted by [op1]. *)
let move op1 op2 limit =
  if limit !outline_pos_x then
    card_start_pos :=
      (op1 (fst !card_start_pos) selected_spacing, snd !card_start_pos)
  else (
    highlight_selection _GOLD _GREEN
      (op2 0 selected_spacing)
      !outline_pos_x !outline_pos_y outline_width outline_height;
    outline_pos_x := op2 !outline_pos_x selected_spacing;
    card_selected_idx := !card_selected_idx + 1)

let gt x = 1 <= x

(* [draw_main_screen] draws all parts of the main screen except player
   hand cards. *)
let draw_main_screen =
  open_window;
  set_background _BLACK;
  draw_logo ();
  draw_game_frames ();
  draw_top_card "Draw4";
  draw_card_deck ();
  set_player_hand_background ();
  highlight_selection _GOLD _GREEN 0 !outline_pos_x !outline_pos_y
    outline_width outline_height

let pp_tuple tuple =
  "("
  ^ string_of_int (fst tuple)
  ^ ", "
  ^ string_of_int (snd tuple)
  ^ ")"

;;
draw_main_screen;
try
  while true do
    draw_cards player_cards !card_start_pos (selected_spacing, 0);
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _RIGHT_KEY then
      (* if !outline_pos_x >= List.length player_cards * fst card_space
         then () *)
      (* if !outline_pos_x >= int_of_string _WIDTH - fst card_space then
         card_start_pos := (fst !card_start_pos - selected_spacing, snd
         !card_start_pos) else ( highlight_selection _GOLD _GREEN
         selected_spacing !outline_pos_x !outline_pos_y outline_width
         outline_height; outline_pos_x := !outline_pos_x +
         selected_spacing; card_selected_idx := !card_selected_idx + 1 ) *)
      move ( - ) ( + ) (fun x ->
          x >= int_of_string _WIDTH - fst card_space)
      (* move_selected ( + ) *)
    else if st.key = _LEFT_KEY then
      (* if !outline_pos_x <= fst !card_start_pos - 10 then () else (
         highlight_selection _GOLD _GREEN ~-selected_spacing
         !outline_pos_x !outline_pos_y outline_width outline_height;
         outline_pos_x := !outline_pos_x + ~-selected_spacing;
         card_selected_idx := !card_selected_idx - 1 ) *)
      move ( + ) ( - ) (fun x -> x <= fst card_init_pos)
    else if st.key = _CONFIRM_KEY then
      failwith
        ("player selected a card: " ^ string_of_int !card_selected_idx)
  done
with Exit -> ()
