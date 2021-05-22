open Graphics
open Constants
open Images
open Png

let open_window =
  open_graph (":0.0 " ^ width ^ "x" ^ height);
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
upload_img card_dir "Green" 250 250

;;
try
  while true do
    let st =
      wait_next_event [ Mouse_motion; Button_down; Key_pressed ]
    in
    synchronize ();
    if st.key = 'q' then raise Exit
  done
with Exit -> ()
