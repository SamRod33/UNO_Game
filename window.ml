open Graphics
open Constants
open Images
open Png

let start_x = ref 240

let start_y = ref 240

let selected_outline_x = ref 300

let selected_outline_y = ref 420

let open_window =
  open_graph (" " ^ width ^ "x" ^ height);
  set_window_title "Uno Game by TKYS"

let upload_img dir file x y =
  let img = Png.load_as_rgb24 (dir ^ file ^ ".png") [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

;;
open_window;
set_color (rgb 0 0 0);
draw_rect 0 0 (int_of_string width) (int_of_string height);
fill_rect 0 0 (int_of_string width) (int_of_string height);
upload_img card_dir "Green" 250 250;
set_color (rgb 248 218 39)

;;
try
  while true do
    let st =
      wait_next_event [ Mouse_motion; Button_down; Key_pressed ]
    in
    synchronize ();
    if st.key = quit_key then raise Exit
    else if st.key = right_key then (
      set_color (rgb 0 0 0);
      draw_rect (!start_x - 400) !start_y !selected_outline_x
        !selected_outline_y;
      set_color (rgb 248 218 39);
      draw_rect !start_x !start_y !selected_outline_x
        !selected_outline_y;
      start_x := !start_x + 400 )
  done
with Exit -> ()
