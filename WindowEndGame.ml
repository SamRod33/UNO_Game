open Graphics
open Constants
open Images
open Png
open WindowGui

(** [end_game_win] displays the End Game Window to the UNO Game. *)
let end_game_win p_id () =
  upload_img _ASSET_DIR "Winner_Game" 0 0;
  display_player_num p_id (430, 365);
  try
    while running do
      let st = wait_next_event [ Key_pressed ] in
      synchronize ();
      if st.keypressed then raise Exit
    done
  with Exit -> ()

;;
open_window;
end_game_win 9 ()
