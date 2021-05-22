open Graphics
open Constants
open Images
open Png
open Window_gui

let _SELECTED_PLAYER_CARD_X = ref 30

let _SELECTED_PLAYER_CARD_Y = ref 25

let selected_spacing = 161

let card_selected_idx = ref 0

let draw_player_cards () =
  (********************** starting x, index of list, selected spacing,
    same y*)
  upload_img _CARD_DIR "Blue-0" (40 + (0 * selected_spacing)) 35;
  upload_img _CARD_DIR "Blue-0" (40 + (1 * selected_spacing)) 35;
  upload_img _CARD_DIR "Blue-0" (40 + (2 * selected_spacing)) 35;
  upload_img _CARD_DIR "Blue-0" (40 + (3 * selected_spacing)) 35;
  upload_img _CARD_DIR "Blue-0" (40 + (4 * selected_spacing)) 35;
  upload_img _CARD_DIR "Blue-0" (40 + (5 * selected_spacing)) 35

let draw_top_card () = upload_img _CARD_DIR "Blue-0" 272 330

let draw_card_deck () = upload_img _CARD_DIR "back" 84 330

let indicate_selected_card () =
  set_color _GOLD;
  draw_rect !_START_X !_START_Y outline_width outline_height

let set_player_hand_background () =
  set_color _GREEN;
  fill_rect 0 0 (int_of_string _WIDTH) 275

let draw_game_frames () =
  upload_img _ASSET_DIR "game_keys" 632 553;
  upload_img _ASSET_DIR "player_turn_frame" 84 553;
  upload_img _ASSET_DIR "penalty_frame" 448 330;
  upload_img _ASSET_DIR "player_hand_frame" 624 330

let move_selected op =
  set_color _GREEN;
  draw_rect
    !_SELECTED_PLAYER_CARD_X
    !_SELECTED_PLAYER_CARD_Y
    outline_width outline_height;
  set_color _GOLD;
  _SELECTED_PLAYER_CARD_X :=
    op !_SELECTED_PLAYER_CARD_X selected_spacing;
  draw_rect
    !_SELECTED_PLAYER_CARD_X
    !_SELECTED_PLAYER_CARD_Y
    outline_width outline_height;
  card_selected_idx := op !card_selected_idx 1

;;
open_window;
set_background _BLACK;
draw_logo ();
draw_game_frames ();
draw_top_card ();
draw_card_deck ();
set_player_hand_background ();
draw_player_cards ();
indicate_selected_card ()

;;
try
  while true do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _RIGHT_KEY then move_selected ( + )
    else if st.key = _LEFT_KEY then move_selected ( - )
    else if st.key = _CONFIRM_KEY then
      failwith
        ("player selected a card: " ^ string_of_int !card_selected_idx)
  done
with Exit -> ()
