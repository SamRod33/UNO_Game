open Graphics
open Constants
open Window_gui

(* [outline_pos_x, outline_pos_y] is the starting position of the
   selection outline. *)
let outline_pos_x, outline_pos_y =
  (ref (cards_start_pos_x - 10), ref (cards_start_pos_y - 10))

let card_selected_idx = ref 0

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
        ( "TODO: return card_selected: "
        ^ string_of_int !card_selected_idx )
    else if st.key = _RIGHT_KEY then
      if
        !outline_pos_x
        >= List.length change_color_cards * fst card_space
      then ()
      else (
        highlight_selection _GOLD _BLACK (fst card_space) !outline_pos_x
          !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + fst card_space;
        card_selected_idx := !card_selected_idx + 1 )
    else if st.key = _LEFT_KEY then
      if !outline_pos_x <= cards_start_pos_x - 10 then ()
      else (
        highlight_selection _GOLD _BLACK
          ~-(fst card_space)
          !outline_pos_x !outline_pos_y outline_width outline_height;
        outline_pos_x := !outline_pos_x + ~-(fst card_space);
        card_selected_idx := !card_selected_idx - 1 )
  done
with Exit -> ()
