open Graphics
open Char

;;
Graphics.open_graph " 640x480"

let height = Graphics.size_y

let width = Graphics.size_x

(*[welcome_screen] is the graphical design for the welcome screen when
  the user first opens the app. I will ask them to enter their name and
  then press tab to contine, where they will be able to choose their own
  customizations for their journal*)
let welcome_screen () =
  Graphics.moveto 180 450;
  Graphics.draw_string "Welcome to your own customizable journal!";
  Graphics.moveto 200 440;
  Graphics.lineto 400 440;
  Graphics.moveto 20 400;
  Graphics.draw_string
    "Please click on your customizations and enter your name";
  Graphics.moveto 356 400;
  Graphics.draw_string "below. Once everything is selected to your";
  Graphics.moveto 20 380;
  Graphics.draw_string
    "liking you may hit enter to create your very own personalized \
     journal";
  Graphics.moveto 185 350;
  Graphics.draw_string "Name:______________________________________";
  Graphics.moveto 200 335;
  Graphics.draw_string "(type your name above then press ENTER)"

(*[add_customization_options] adds more graphical interface options to
  the welcome screen when the user presses tab. It makes it so that they
  may choose the color of the background and text, and when they press
  enter brings them to their very own customized journal*)
let add_customization_options () =
  Graphics.moveto 484 10;
  Graphics.draw_string "Press ENTER to continue"

let text_options () =
  Graphics.moveto 50 305;
  Graphics.draw_string "Click a color for the text:";
  Graphics.moveto 50 285;
  Graphics.draw_string
    "black      red      green      blue     yellow      cyan      \
     magenta      white"

let background_options () =
  Graphics.moveto 50 245;
  Graphics.draw_string "Click a color for the background:";
  Graphics.moveto 50 225;
  Graphics.draw_string
    "black      red      green      blue     yellow      cyan      \
     magenta      white"

let line_options () =
  Graphics.moveto 50 185;
  Graphics.draw_string "Click a color for the lines:";
  Graphics.moveto 50 165;
  Graphics.draw_string
    "black      red      green      blue     yellow      cyan      \
     magenta      white"

let example_display () =
  Graphics.set_color black;
  Graphics.fill_rect 250 25 140 110;
  Graphics.set_color 0xCCFFFF;
  (*users background color*)
  Graphics.fill_rect 255 30 130 100;
  Graphics.moveto 275 30;
  Graphics.set_color 0xFFCCFF;
  Graphics.lineto 275 130;
  Graphics.set_color black;
  (*users line color*)
  Graphics.moveto 255 63;
  Graphics.lineto 385 63;
  Graphics.moveto 255 96;
  Graphics.lineto 385 96;
  Graphics.moveto 293 97;
  Graphics.set_color black;
  (*users text color*)
  Graphics.draw_string "example";
  Graphics.moveto 293 64;
  Graphics.draw_string "window"

(*[draw_journal] is the graphical design for the journal that the user
  creates. It brings up their cutomized journal in the colors they
  select along with their name displayed at the top of the journal*)
let draw_journal () =
  Graphics.set_color magenta;
  Graphics.set_text_size 30;
  Graphics.moveto 270 450;
  Graphics.draw_string "My Journal";
  Graphics.moveto 200 440;
  Graphics.lineto 400 440;

  Graphics.set_color black;

  for i = 0 to 12 do
    Graphics.moveto 0 (height () - 80 - (30 * i));
    Graphics.lineto (width ()) (height () - 80 - (30 * i))
  done;

  Graphics.set_color magenta;
  Graphics.moveto 30 0;
  Graphics.lineto 30 (height ());

  (* waits for event to take in characters *)
  Graphics.set_color black

;;
welcome_screen ()

exception End

let tab_size = 48

(******************************************************************************
  This section is just stuff im playing around with information on
  creating clickable buttons to implement on the main screen of the
  journal
  ******************************************************************************)

(*The first part of this object, (int*int), represents the bottom right
  cornor of the button. The string represents the name of the button and
  the color is the background color*)
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
  && x <= (String.length name * 12) + 48 + a
  && y >= b
  && y <= 28 + b

let exit_button () : button = ((100, 450), "Color", yellow)
(******************************************************************************
  end
  ******************************************************************************)

(*[adjust_mouse_y] is used on the journal screen so that when the user
  clicks a location on the screen, it is modified to be on a line*)
let rec adjust_mouse_y mouse_y =
  if mouse_y > 400 then 400
  else if (400 - mouse_y) mod 30 != 0 then adjust_mouse_y (mouse_y - 1)
  else mouse_y

(*[adjust_mouse_x] is used on the journal screen so that when the user
  clicks a location on the screen, it is modified to be on a line*)
let rec adjust_mouse_x mouse_x =
  if mouse_x < 35 then 35
  else if (35 - mouse_x) mod 6 = 0 then mouse_x
  else adjust_mouse_x (mouse_x - 1)

let next_line () =
  let x, y = Graphics.current_point () in
  if y > 12 then Graphics.moveto 35 (y - 30) else Graphics.moveto 35 400

let prev_line () =
  let x, y = Graphics.current_point () in
  if y < 400 then Graphics.moveto (width () - 18) (y + 30)
  else Graphics.moveto (width ()) (400 mod 30)

let backspace_char init_pos_x init_pos_y =
  Graphics.set_color white;
  if init_pos_x > 37 then
    Graphics.fill_rect (init_pos_x - 6) (init_pos_y + 1) 5 10
  else Graphics.fill_rect (width () - 6) (init_pos_y + 31) 5 10;
  Graphics.set_color black;
  if init_pos_x > 37 then Graphics.moveto (init_pos_x - 6) init_pos_y
  else prev_line ()

let delete_char init_pos_x init_pos_y =
  Graphics.set_color white;
  Graphics.fill_rect init_pos_x (init_pos_y + 1) 5 10;
  Graphics.set_color black

(*[update_string] takes a character and appends it to a string*)
let update_string old_string new_char =
  old_string ^ Char.escaped new_char

let handle_char_journal c =
  match c with
  | '\x1B' -> raise End
  | '\n' -> next_line ()
  | '\r' -> next_line ()
  | '\b' ->
      let x, y = Graphics.current_point () in
      backspace_char x y
  | '\t' ->
      let x, y = Graphics.current_point () in
      Graphics.moveto (x + tab_size) y
  | '\127' ->
      let x, y = Graphics.current_point () in
      if x > 35 then delete_char x y
  | _ -> Graphics.draw_char c

let handle_char_welcome c =
  match c with
  | '\x1B' -> raise End
  | '\n' ->
      Graphics.clear_graph ();
      draw_journal ()
  | '\r' ->
      Graphics.clear_graph ();
      draw_journal ()
  | _ -> Graphics.moveto 0 0

(*[start_sequence_journal] will display the graphical interface that is
  a journal and that allows the user to type their text onto the journal*)
let start_sequence_journal f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    while true do
      try
        let s =
          Graphics.wait_next_event
            [ Graphics.Button_down; Graphics.Key_pressed ]
        in
        if s.Graphics.keypressed then (
          let x, y = Graphics.current_point () in
          if x > width () - 18 then next_line ();
          f_key s.Graphics.key)
        else if s.Graphics.button then
          f_mouse
            (adjust_mouse_x s.Graphics.mouse_x)
            (adjust_mouse_y s.Graphics.mouse_y)
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

(*[restore_background_colors] makes it so that the none of the
  selections for the background colors are hilighted*)
let restore_colors y_pos =
  Graphics.set_color white;
  Graphics.fill_rect 45 y_pos 500 12;
  Graphics.set_color black

let restore_background_colors () =
  restore_colors 225;
  background_options ()

let restore_text_colors () =
  restore_colors 285;
  text_options ()

let restore_line_colors () =
  restore_colors 165;
  line_options ()

(*[highlight_helper] draws a rectangle that is filled with the color
  [hex] and writes the string [color] on top of that, to give the effect
  of highlighting the word*)
let highlight_helper color hex x y_min y_max =
  Graphics.set_color hex;
  Graphics.fill_rect (x - 2) y_min ((String.length color * 6) + 2) 12;
  if hex = white || hex = black then Graphics.set_color white
  else Graphics.set_color black;
  Graphics.moveto x y_min;
  Graphics.draw_string color

(*[highlight_selected] will be used to highlight the color that the user
  selects so that they know what color they have chosen*)
let highlight_selected (x : int) (y : int) y_min y_max =
  if x > 50 && x < 80 && y > y_min && y < y_max then
    (*User_info.set_background_color "black";*)
    highlight_helper "black" black 50 y_min y_max
  else if x > 116 && x < 134 && y > y_min && y < y_max then
    highlight_helper "red" 0xFFCCCC 116 y_min y_max
  else if x > 170 && x < 200 && y > y_min && y < y_max then
    highlight_helper "green" 0xCCFFCC 170 y_min y_max
  else if x > 236 && x < 260 && y > y_min && y < y_max then
    highlight_helper "blue" 0xCCCCFF 236 y_min y_max
  else if x > 290 && x < 332 && y > y_min && y < y_max then
    highlight_helper "yellow" 0xFFFFCC 290 y_min y_max
  else if x > 362 && x < 387 && y > y_min && y < y_max then
    highlight_helper "cyan" 0xCCFFFF 362 y_min y_max
  else if x > 422 && x < 465 && y > y_min && y < y_max then
    highlight_helper "magenta" 0xFFCCFF 422 y_min y_max
  else if x > 500 && x < 530 && y > y_min && y < y_max then
    highlight_helper "white" black 500 y_min y_max
  else Graphics.set_color black

let highlight_selected_background x y = highlight_selected x y 225 235

let highlight_selected_text_color x y = highlight_selected x y 285 295

let highlight_selected_line_color x y = highlight_selected x y 165 175

(*[start_sequence_welcome] will be used to gather the input of the users
  color choices and when they hit enter will display the journal to them*)
let start_sequence_welcome f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    let x = ref true in
    while !x do
      try
        let s =
          Graphics.wait_next_event
            [ Graphics.Button_down; Graphics.Key_pressed ]
        in
        if s.Graphics.keypressed then (
          f_key s.Graphics.key;
          if s.Graphics.key = '\r' then (
            x := false;
            start_sequence_journal
              (fun () -> Graphics.moveto 35 400)
              f_end handle_char_journal f_mouse f_except))
        else if s.Graphics.button then
          f_mouse s.Graphics.mouse_x s.Graphics.mouse_y;
        if s.Graphics.mouse_y > 225 && s.Graphics.mouse_y < 235 then (
          restore_background_colors ();
          highlight_selected_background s.Graphics.mouse_x
            s.Graphics.mouse_y)
        else if s.Graphics.mouse_y > 285 && s.Graphics.mouse_y < 295
        then (
          restore_text_colors ();
          highlight_selected_text_color s.Graphics.mouse_x
            s.Graphics.mouse_y)
        else if s.Graphics.mouse_y > 165 && s.Graphics.mouse_y < 175
        then (
          restore_line_colors ();
          highlight_selected_line_color s.Graphics.mouse_x
            s.Graphics.mouse_y)
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

(*[start_sequence_welcome] will be used to gather the input of the users
  name and save it so that it can later be displayed*)
let start_sequence_name f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    let x = ref true in
    while !x do
      try
        let s =
          Graphics.wait_next_event
            [ Graphics.Button_down; Graphics.Key_pressed ]
        in
        if s.Graphics.keypressed then (
          f_key s.Graphics.key;
          if s.Graphics.key = '\r' then (
            x := false;
            start_sequence_welcome f_init f_end handle_char_welcome
              f_mouse f_except))
        else if s.Graphics.button then Graphics.set_color black
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

(*[handle_char_name] makes it so when the user is on the welcome screen
  pressing enter advances them to the next steps*)
let handle_char_name c =
  match c with
  | '\x1B' -> raise End
  | '\t' -> Graphics.set_color black
  | '\n' ->
      add_customization_options ();
      text_options ();
      background_options ();
      line_options ();
      example_display ()
  | '\r' ->
      add_customization_options ();
      text_options ();
      background_options ();
      line_options ();
      example_display ()
  | '\b' ->
      let x, y = Graphics.current_point () in
      backspace_char x y
  | '\127' -> Graphics.set_color black
  | _ -> Graphics.draw_char c

let start () =
  start_sequence_name
    (fun () -> Graphics.moveto 227 351)
    (fun () -> Graphics.clear_graph ())
    handle_char_name
    (fun x y -> Graphics.moveto x y)
    (fun e -> ())

;;
start ()

(*let txt_to_string txt = let ch = open_in txt in let s =
  really_input_string ch (in_channel_length ch) in close_in ch; s

  let rec interactive () = let event = wait_next_event [Key_pressed] in
  if event.key == '.' then exit 0 else Graphics.draw_char event.key;

  let s = update_string "" c in Graphics.draw_string s *)
