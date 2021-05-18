open Graphics
open Images
open Png

;;
Graphics.open_graph " 800x680"

let height = Graphics.size_y

let width = Graphics.size_x

let px_size = 16

let scaling = ref 2

let open_window =
  try
    while true do
      ()
    done
  with _ -> Graphics.clear_graph ()

exception Exit

let run_uno =
  loop_at_exit [ Key_pressed ] (fun event ->
      if event.key = 'q' then raise Exit;
      if event.keypressed then open_window)

(*most of this is very similar to pokemon, must change*)
let img_arr img_t =
  let graph_arr =
    match img_t with
    | Rgba32 bits ->
        let h = bits.Rgba32.height in
        let w = bits.Rgba32.width in
        Array.init h (fun heights ->
            Array.init w (fun widths ->
                let { color = { r; g; b }; alpha = tr } =
                  Rgba32.unsafe_get bits widths heights
                in
                rgb r g b))
    | _ -> failwith "never"
  in
  let list_img = Array.(map Array.to_list graph_arr |> to_list) in
  let rec nappend a b n = nappend (Array.append a [| b |]) b (n - 1) in
  List.fold_left
    (fun acc row ->
      nappend acc
        (List.fold_left
           (fun racc color -> nappend racc color 1)
           [||] row)
        1)
    [||] list_img

let upload_img folder file x y =
  let img = Png.load (folder ^ file ^ ".png") [] in
  let draw = img |> img_arr |> Graphics.make_image in
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

(*[clicked] returns true if the mouses location (x,y) is within the
  parameters of the button*)
let clicked x y button =
  let (a, b), name, color = button in
  x >= a
  && x <= (String.length name * 6) + 24 + a
  && y >= b
  && y <= 14 + b

let exit_button () : button = ((100, 450), "Exit", red)

;;
run_uno;
upload_img "card" "Green" 50 50
