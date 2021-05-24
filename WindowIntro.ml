open Graphics
open Constants
open Images
open Png
open WindowGui

let intro_txt_x, intro_txt_y = (logo_pos_x, logo_pos_y - 100)

let intro_win =
  upload_img _ASSET_DIR "Start_Game" 0 0;
  try
    while running do
      let st = wait_next_event [ Key_pressed ] in
      synchronize ();
      (* to simulate any possible keyboard press *)
      match st.key with '\000' -> () | _ -> raise Exit
    done
  with Exit -> ()

;;
open_window;
intro_win
