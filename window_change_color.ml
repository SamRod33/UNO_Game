open Graphics
open Constants
open Window_gui

(* [outline_width, outline_height] is the dimensions of the outline. *)
let outline_width, outline_height = (145, 200)

(* [cards_start_pos_x, cards_start_pos_y] is the starting position of
   the color changing cards, as inspired by the mockup. *)
let cards_start_pos_x, cards_start_pos_y =
  (change_c_txt_pos_x - 200, change_c_txt_pos_y - 300)

(* [outline_pos_x, outline_pos_y] is the starting position of the
   selection outline. *)
let outline_pos_x, outline_pos_y =
  (ref (cards_start_pos_x - 10), ref (cards_start_pos_y - 10))

let card_selected_idx = ref 0

let upload_img dir file x y =
  let img = Png.load_as_rgb24 (dir ^ file) [] in
  let draw = Graphic_image.of_image img in
  Graphics.draw_image draw x y

let change_color_cards =
  [
    "red_color.png";
    "blue_color.png";
    "green_color.png";
    "yellow_color.png";
  ]

(** [draw_card c pos] draws [c] at [pos]. *)
let draw_card c pos =
  let c_x, c_y = pos in
  print_endline c;
  upload_img _ASSET_DIR c c_x c_y

(** [draw_cards cards pos] draws [cards] starting at [pos]. Ensures
    spacing between each card. *)
let rec draw_cards cards pos =
  let c_x, c_y = pos in
  let c_space_x, c_space_y = card_space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_card c pos;
      draw_cards t (c_x + c_space_x, c_y + c_space_y)

let draw_change_color_screen () =
  (* TODO: will need to omit the open_window when integrating. *)
  open_window;
  set_background _BLACK;
  draw_logo ();
  draw_txt "CHOOSE A NEW COLOR";
  draw_cards change_color_cards (cards_start_pos_x, cards_start_pos_y)

;;
open_window;
draw_change_color_screen ();
highlight_selection _GOLD _BLACK 0 !outline_pos_x !outline_pos_y
  outline_width outline_height;

try
  while running do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _CONFIRM_KEY then
      failwith
        ("TODO: return card_selected: "
        ^ string_of_int !card_selected_idx)
    else if st.key = _RIGHT_KEY then
      if
        !outline_pos_x
        >= List.length change_color_cards * fst card_space
      then ()
      else (
        highlight_selection _GOLD _BLACK (fst card_space) !outline_pos_x
          !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + fst card_space;
        card_selected_idx := !card_selected_idx + 1)
    else if st.key = _LEFT_KEY then
      if !outline_pos_x <= cards_start_pos_x - 10 then ()
      else (
        highlight_selection _GOLD _BLACK
          ~-(fst card_space)
          !outline_pos_x !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + ~-(fst card_space);
        card_selected_idx := !card_selected_idx - 1)
  done
with Exit -> ()
