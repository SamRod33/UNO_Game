open Graphics
open Constants
open Images
open Png
open Window_gui

(* Sam,Yohanes, Keri 176 matches the design and spacing for this window*)
let card_space = (176, snd card_space)

let display_player_num p (x, y) =
  upload_img _TEXT_DIR (string_of_int p) x y

(**********************************************************************)
(*functions will be replaced once we integrate info*)
let draw_img c pos =
  let c_x, c_y = pos in
  upload_img _CARD_DIR c c_x c_y

let rec draw_images cards pos =
  let c_x, c_y = pos in
  let c_space_x, c_space_y = card_space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_img c pos;
      draw_images t (c_x + c_space_x, c_y + c_space_y)

let dummy_data = [ "Blue-0"; "Green-0"; "Yellow-0"; "Red-0"; "Blue-0" ]

(**********************************************************************)

;;
open_window;
set_background _BLACK;
upload_img _TEXT_DIR "Click any key to continue" 333 686;
upload_img _ASSET_DIR "player_turn_frame" 371 466;
upload_img _TEXT_DIR "Most recently played cards" 105 378;
upload_img _TEXT_DIR "New" 108 301;
upload_img _TEXT_DIR "Old" 816 301;
display_player_num 5 (600, 520);
draw_images dummy_data (105, 101)

;;
try
  while true do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.keypressed then raise Exit
  done
with Exit -> ()
