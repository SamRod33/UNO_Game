open Graphics
open Constants
open Images
open Png
open WindowGui

(** [help_win ()] displays the Help Window to the UNO Game. *)
let help_win () =
  upload_img _ASSET_DIR "Help_Game" 0 0;
  try
    while running do
      let st = wait_next_event [ Key_pressed ] in
      synchronize ();
      if st.keypressed then raise Exit
    done
  with Exit -> ()

(* ;; open_window; help_win () *)
