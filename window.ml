open Graphics
open Images
open Png

let () = Graphics.open_graph ""

let height = Graphics.size_y

let width = Graphics.size_x

exception Exit

(* let open_window = try while true do (* let x = !x + 1 in
   Graphics.draw_image g x 0 *) () done with Exit -> () *)

(* let run_uno = loop_at_exit [ Key_pressed ] (fun event -> if event.key
   = 'q' then raise Exit; if event.keypressed then open_window)*)

let upload_img file x y =
  let img = Png.load_as_rgb24 (file ^ ".png") [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

let clicked x y button =
  let (a, b), name, color = button in
  x >= a
  && x <= (String.length name * 6) + 24 + a
  && y >= b
  && y <= 14 + b

(** Type [button] has an integer pair [(int * int)] that represents the
    bottom right corner of the button. [string] is the text displayed
    within the button. [color] is the button's background color. *)
type button = (int * int) * string * color

let exit_button () : button = ((100, 450), "Exit", red)

let draw_button (button : button) : unit =
  let (x, y), text, color = button in
  Graphics.set_color color;
  Graphics.fill_rect x y ((String.length text * 12) + 48) 28;
  Graphics.set_color black;
  Graphics.draw_rect x y ((String.length text * 12) + 48) 28;
  Graphics.moveto (x + 24) (y + 4);
  Graphics.draw_string text

;;
upload_img "Green" 0 0

;;
draw_button ((50, 50), "Exit", red)

(* open_window *)

;;
Unix.sleep 100
