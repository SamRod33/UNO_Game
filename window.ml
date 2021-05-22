open Graphics
open Constants
open Images
open Png

let open_window =
  open_graph (":0.0 " ^ _WIDTH ^ "x" ^ _HEIGHT);
  set_window_title "Uno Game by TKYS"

let upload_img dir file x y =
  let img = Png.load_as_rgb24 (dir ^ file ^ ".png") [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

;;
open_window;
set_color (rgb 0 0 0);
fill_rect 0 0 (int_of_string _WIDTH) (int_of_string _HEIGHT);
upload_img _ASSET_DIR "game_keys" 632 553;
upload_img _ASSET_DIR "gui_uno_logo" 424 578;
upload_img _ASSET_DIR "player_turn_frame" 84 553;

upload_img _ASSET_DIR "penalty_frame" 448 330;
upload_img _ASSET_DIR "player_hand_frame" 624 330;
upload_img _CARD_DIR "Blue-0" 272 330;
upload_img _CARD_DIR "back" 84 330;
set_color (rgb 0 116 70);
fill_rect 0 0 (int_of_string _WIDTH) 275;
upload_img _CARD_DIR "Blue-0" (40 + (0 * 161)) 35;
upload_img _CARD_DIR "Blue-0" (40 + (1 * 161)) 35;
upload_img _CARD_DIR "Blue-0" (40 + (2 * 161)) 35;
upload_img _CARD_DIR "Blue-0" (40 + (3 * 161)) 35;
upload_img _CARD_DIR "Blue-0" (40 + (4 * 161)) 35;
upload_img _CARD_DIR "Blue-0" (40 + (5 * 161)) 35;
set_color (rgb 248 218 39);
draw_rect 36 25 120 180

;;
try
  while true do
    let st =
      wait_next_event [ Mouse_motion; Button_down; Key_pressed ]
    in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _RIGHT_KEY then (
      set_color (rgb 0 116 70);
      draw_rect !_START_X !_START_Y !_SELECTED_OUTLINE_X
        !_SELECTED_OUTLINE_Y;
      set_color (rgb 248 218 39);
      _START_X := !_START_X + 161;
      draw_rect !_START_X !_START_Y !_SELECTED_OUTLINE_X
        !_SELECTED_OUTLINE_Y )
    else if st.key = _LEFT_KEY then (
      set_color (rgb 0 116 70);
      draw_rect !_START_X !_START_Y !_SELECTED_OUTLINE_X
        !_SELECTED_OUTLINE_Y;
      set_color (rgb 248 218 39);
      _START_X := !_START_X - 161;
      draw_rect !_START_X !_START_Y !_SELECTED_OUTLINE_X
        !_SELECTED_OUTLINE_Y )
  done
with Exit -> ()
