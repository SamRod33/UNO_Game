open Graphics
open Constants

let open_window =
  open_graph (":0.0 " ^ _WIDTH ^ "x" ^ _HEIGHT);
  set_window_title "Uno Game by TKYS"

let running = true

(* [logo_pos_x, logo_pos_y] is the position of the UNO Logo, as inspired
   by the mockup. *)
let logo_pos_x, logo_pos_y = (424, 578)

(* [change_swap_txt_pos_x, change_swap_txt_pos_y] is the position of the
   change color prompt, as inspired by the mockup. *)
let change_swap_txt_pos_x, change_swap_txt_pos_y =
  (logo_pos_x, logo_pos_y - 100)

(* [cards_start_pos_x, cards_start_pos_y] is the starting position of
   the color changing cards, as inspired by the mockup. *)
let cards_swap_start_pos_x, cards_swap_start_pos_y =
  (change_swap_txt_pos_x - 250, change_swap_txt_pos_y - 300)

(* [card_space] is the amount of relative spacing between cards aligned
   horizontally. *)
let card_space = (150, 0)

(* [swap_h_space] is the amount of spacing between the left edges of two
   swap info cards aligned horizontally. *)
let swap_space = (112 + 57, 0)

(** [set_background color] fills the background of the window with
    [color]. *)
let set_background color =
  set_color color;
  fill_rect 0 0 (int_of_string _WIDTH) (int_of_string _HEIGHT)

let upload_img dir file x y =
  let img = Png.load_as_rgb24 (dir ^ file ^ ".png") [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

(** [draw_logo ()] draws the logo on the screen. *)
let draw_logo () =
  upload_img _ASSET_DIR "gui_uno_logo" logo_pos_x logo_pos_y

(** [draw_card c pos] draws [c] at [pos]. *)
let draw_card c pos =
  let c_x, c_y = pos in
  upload_img _CARD_DIR (Card.img c) c_x c_y

(** [draw_cards cards pos space] draws [cards] starting at [pos].
    Separates each [space]. *)
let rec draw_cards cards pos space =
  let c_x, c_y = pos in
  let c_space_x, c_space_y = space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_card c pos;
      draw_cards t (c_x + c_space_x, c_y + c_space_y) space

(** [highlight_selection color_on color_off offset x y width height]
    draws an colors old outline with [color_off] at ([x], [y]), and
    draws a new outline horizontally shifted by [offset]. Both drawing
    are rectangles of dimension [width] x [height]. *)
let highlight_selection color_on color_off offset x y width height =
  set_color color_off;
  draw_rect x y width height;
  set_color color_on;
  draw_rect (x + offset) y width height

let display_player_num p (x, y) =
  upload_img _TEXT_DIR (string_of_int p) x y

let rec display_num i (x, y) =
  if i < 10 then upload_img _TEXT_DIR (string_of_int i) x y
  else if i < 100 then (
    let fst_dig = i / 10 in
    let snd_dig = i mod 10 in
    upload_img _TEXT_DIR (string_of_int fst_dig) x y;
    display_num snd_dig (x + 32, y))
