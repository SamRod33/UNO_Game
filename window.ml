open Graphics
open Images
open Png

let () = Graphics.open_graph ""

let height = Graphics.size_y

let width = Graphics.size_x

let px_size = 16

let scaling = ref 2

exception Exit

exception End

let open_window =
  try
    print_string "true";
    ()
  with _ ->
    print_string "errored";
    Graphics.clear_graph ()

let run_uno =
  loop_at_exit [ Key_pressed ] (fun event ->
      if event.key = 'q' then raise Exit;
      if event.keypressed then open_window)

let upload_img folder file x y =
  let img = Png.load_as_rgb24 (folder ^ file ^ ".png") [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

(***************buttons****************)

(** Type [button] has an integer pair [(int * int)] that represents the
    bottom right corner of the button. [string] is the text displayed
    within the button. [color] is the button's background color. *)
type button = (int * int) * string * color

(*[draw_button button] is a function that takes in a button and displays
  it on the screen*)
let draw_button (button : button) : unit =
  let (x, y), text, color = button in
  Graphics.set_color color;
  Graphics.fill_rect x y ((String.length text * 12) + 48) 28;
  Graphics.set_color black;
  Graphics.draw_rect x y ((String.length text * 12) + 48) 28;
  Graphics.moveto (x + 24) (y + 4);
  Graphics.draw_string text

(* cards; get orientation (colormap) of pixels for basic card, change
   background color*)

let clicked x y button =
  let (a, b), name, color = button in
  x >= a
  && x <= (String.length name * 6) + 24 + a
  && y >= b
  && y <= 14 + b

let exit_button () : button = ((100, 450), "Exit", red)

(*run_uno; ;; draw_button ((50, 50), "Exit", red); upload_img "card"
  "Green" 200 200; Unix.sleep 100*)

let img = Png.load_as_rgb24 "Green.png" []

let g = Graphic_image.of_image img

;;
Graphics.draw_image g 0 0

;;
draw_button ((50, 50), "Exit", red)

;;
Unix.sleep 100
