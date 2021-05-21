(* open Graphics

   ;; open_graph " 480x270" ;;

   ;; (* set_color (rgb 0 255 0); *) remember_mode false; try while true
   do let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed
   ] in synchronize (); if st.keypressed then raise Exit; if st.button
   then ( remember_mode true; remember_mode false ); let x = st.mouse_x
   + 16 and y = st.mouse_y + 16 in moveto 0 y; (* lineto (x - 25) y; *)
   moveto 10000 y; (* lineto (x + 25) y; *) moveto x 0; (* lineto x (y -
   25); *) moveto x 10000 (* lineto x (y + 25) *) done with Exit -> () *)

open Graphics

(** Type [button] has an integer pair [(int * int)] that represents the
    bottom right corner of the button. [string] is the text displayed
    within the button. [color] is the button's background color. *)
type button = (int * int) * string * color

(* ([draw_button button] is a function that takes in a button and
   displays it on the screen) *)
let draw_button (button : button) : unit =
  let (x, y), text, color = button in
  Graphics.set_color color;
  Graphics.fill_rect x y ((String.length text * 12) + 48) 28;
  Graphics.set_color black;
  Graphics.draw_rect x y ((String.length text * 12) + 48) 28;
  Graphics.moveto (x + 24) (y + 4);
  Graphics.draw_string text

open Images

let () = Graphics.open_graph ""

let img = Png.load_as_rgb24 "Green.png" []

let g = Graphic_image.of_image img

;;
Graphics.draw_image g 0 0

;;
draw_button ((50, 50), "Exit", red)

;;
Unix.sleep 100
