open Graphics
open Constants
open Images
open Png
open WindowGui
open State
open Player

(*Overrides x spacing constant for intermssion window*)
let card_space = (176, snd WindowGui.card_space)

(**********************************************************************)
(*These functions are here testing and modular development purposes*)

(** [draw_img c pos] is the image [c] drawn at position ([x], [y]). *)
let draw_img c pos =
  let c_x, c_y = pos in
  upload_img _CARD_DIR c c_x c_y

(** [draw_images cards pos] is the images [cards] drawn starting from
    position ([x], [y]). *)
let rec draw_images cards pos =
  let c_x, c_y = pos in
  let c_space_x, c_space_y = card_space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_img (Card.img c) pos;
      draw_images t (c_x + c_space_x, c_y + c_space_y)

let dummy_data = [ "Blue-0"; "Green-0"; "Yellow-0"; "Red-0"; "Blue-0" ]

(**********************************************************************)

(** [draw_intermission_window ()] draws the images used in the
    intermission window. *)
let draw_intermission_window () =
  set_background _BLACK;
  upload_img _TEXT_DIR "Click any key to continue" 333 686;
  upload_img _ASSET_DIR "player_turn_frame" 371 466;
  upload_img _TEXT_DIR "Most recently played cards" 105 378;
  upload_img _TEXT_DIR "New" 108 301;
  upload_img _TEXT_DIR "Old" 816 301

(** [intermission_phase st g] launches the intermission window phase. *)
let intermission_phase st g card_list =
  if st.key = _QUIT_KEY then exit 0
  else if st.key = _CONFIRM_KEY then failwith "hi"
  else open_window;
  draw_intermission_window ();
  display_player_num (id (current_player g)) (600, 520);
  draw_cards !card_list (105, 101) card_space

(** [intermit_win id cards] runs the intermission window of player [id]
    with the five most recent [cards]. *)
let intermit_win id cards =
  draw_intermission_window ();
  display_player_num id (600, 520);
  draw_images cards (105, 101);
  try
    while true do
      let st = wait_next_event [ Key_pressed ] in
      synchronize ();
      if st.keypressed then raise Exit
    done
  with Exit -> ()

(* ;; open_window; intermit_win 4 dummy_data *)
