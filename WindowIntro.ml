open Graphics
open Constants
open Images
open Png
open WindowGui
open WindowHelp

let intro_txt_x, intro_txt_y = (logo_pos_x, logo_pos_y - 100)

(** [intro_win] displays the Intro Window to the UNO Game. *)
let intro_win =
  upload_img _ASSET_DIR "Start_Game" 0 0;
  try
    while running do
      let st = wait_next_event [ Key_pressed ] in
      synchronize ();
      if st.keypressed then (
        help_win ();
        raise Exit)
    done
  with Exit -> ()

;;
open_window;
intro_win
