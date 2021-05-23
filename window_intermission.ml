open Graphics
open Constants
open Images
open Png
open Window_gui
open State
open_window Player

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

let card_space = (176, snd card_space)

let draw_intermission_window () =
  set_background _BLACK;
  upload_img _TEXT_DIR "Click any key to continue" 333 686;
  upload_img _ASSET_DIR "player_turn_frame" 371 466;
  upload_img _TEXT_DIR "Most recently played cards" 105 378;
  upload_img _TEXT_DIR "New" 108 301;
  upload_img _TEXT_DIR "Old" 816 301

(* [change_color_phase st] Launches the change color window phase. *)
let change_color_phase st g =
  if st.key = _QUIT_KEY then raise Exit
  else if st.key = _CONFIRM_KEY then failwith "hi"
  else open_window;
  draw_intermission_window ();
  display_player_num (id (current_player g)) (600, 520)
  draw_cards (top_card g) (105, 101) card_space

;;
open_window;
draw_intermission_window ();
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
