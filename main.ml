open Card

(** [to_string_color c] is the color of the text. *)
let to_string_color c =
  let cc = color c in
  match cc with
  | R -> ANSITerminal.red
  | G -> ANSITerminal.green
  | B -> ANSITerminal.blue
  | Y -> ANSITerminal.yellow
  | ANY -> ANSITerminal.white

(** [to_card_face c] is the face of the card [c]. *)
let to_card_face c =
  let ac = actions c in
  if ac.skip = true then "ⓧ"
  else if ac.reverse = true then "R"
  else if ac.swap = true then "↔"
  else if ac.change_color = true then "C"
  else string_of_int (Option.get (Card.digit c))

(** [pp_card c] is the card [c] pretty printed. *)
let pp_card c =
  let col = to_string_color c in
  ANSITerminal.print_string [ col ] "\n**********\n";
  ANSITerminal.print_string [ col ] "\n*        *\n";
  ANSITerminal.print_string [ col ] "\n*    ";
  ANSITerminal.print_string [ col ] (to_card_face c);
  ANSITerminal.print_string [ col ] "    *\n";
  ANSITerminal.print_string [ col ] "\n*        *\n";
  ANSITerminal.print_string [ col ] "\n**********\n\n";
  ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\nWelcome to the UNO Game engine.\n";
  print_endline "Please enter \"start\" to load cards.\n";
  print_string "> ";
  match read_line () with
  | "start" -> List.iter pp_card Card.standard_cards
  | _ -> print_string "Command not accepted."

(* Execute the game engine. *)
let () = main ()
