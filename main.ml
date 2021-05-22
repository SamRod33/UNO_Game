open State
open Card
open Player
open Computer

(** [create_players num_real num_computer] is a list of
    [num_real + num_computer] players, where [num_computer] are
    computers.*)
let create_players players num_real num_computer =
  let rec help lst cpus = function
    | 0 -> lst
    | n ->
        let player =
          if cpus then create ("Computer " ^ string_of_int n) true
          else create ("Player " ^ string_of_int n) false
        in
        help (player :: lst) cpus (n - 1)
  in
  let people = help [] true num_computer in
  help people false num_real

let player_hand_info player =
  name player ^ " has "
  ^ string_of_int (List.length (player_hand player))
  ^ " cards. "

let player_hands_info player g =
  List.fold_left
    (fun str p -> str ^ player_hand_info p)
    ""
    (List.filter (fun p -> id p <> id player) (players g))

let quit_str = "Thanks for playing!\n\n"

let check_quit () =
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | a -> int_of_string_opt a

let clear () = ignore (Sys.command "clear")

let buffer next_gst =
  clear ();
  print_string
    ("It is "
    ^ name (current_player next_gst)
    ^ "'s turn. Enter anything to continue.\n");
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | _ -> clear ()

let fail_str = "Try again.\n"

let illegal_card = "You can't play that card. Try another.\n"

let rec select_color () =
  print_string "Type in R, G, B, or Y to select the color.\n\n";
  match read_line () with
  | "Quit" ->
      print_string "Thanks for playing!\n\n";
      exit 0
  | "R" -> R
  | "G" -> G
  | "B" -> B
  | "Y" -> Y
  | _ ->
      print_string "Choose a valid color.\n\n";
      select_color ()

let rec game_loop g =
  let cur_player = current_player g in
  if is_cpu cur_player then cpu_play g cur_player
  else player_play g cur_player

and player_play g cur_player =
  let cur_player_hand = player_hand cur_player in
  print_string ("It is " ^ name cur_player ^ "'s turn.\n");
  print_string "The top card is:\n\n";
  pp_cards [ top_card g ] false;
  print_string
    ("\nThe current stack penalty is "
    ^ string_of_int (stack_penalty g)
    ^ ".\n");
  print_string (player_hands_info cur_player g);
  print_string "\nYour cards are:\n\n";
  pp_cards cur_player_hand true;
  print_endline
    "\n\
     Type in the index of the card you wish to play (starting from 0). \
     If you do not have a card to play, type -1.\n";
  print_string "> ";
  end_of_game_loop g cur_player cur_player_hand

and end_of_game_loop g cur_player cur_player_hand =
  match check_quit () with
  | None ->
      clear ();
      print_string fail_str;
      game_loop g
  | Some n ->
      if n > -2 && n < List.length cur_player_hand then (
        let play_card =
          if n = -1 then None else Some (List.nth cur_player_hand n)
        in
        match play play_card g with
        | Illegal ->
            clear ();
            print_string illegal_card;
            game_loop g
        | Legal next_g ->
            clear ();
            format_card g next_g play_card
        | GameOver winner ->
            print_string ("\n" ^ name winner ^ " wins!\n\n");
            exit 0)
      else clear ();
      print_string fail_str;
      game_loop g

and cpu_play g cur_player =
  let cpu_card = action g in
  let changed_gst =
    if
      Option.is_some cpu_card
      && (actions (Option.get cpu_card)).change_color
    then
      change_current_players_hand
        (change_color (Option.get cpu_card) ANY)
        (Option.get cpu_card) g
    else g
  in
  match play cpu_card changed_gst with
  | Legal new_gst -> format_card changed_gst new_gst cpu_card
  | Illegal -> print_string "The computer made an error!\n\n"
  | GameOver _ ->
      print_string "The computer wins...\n\n";
      exit 0

and format_card gst next_gst = function
  | None ->
      buffer next_gst;
      game_loop next_gst
  | Some c ->
      if color c = ANY then
        let new_c = change_color c (select_color ()) in
        match
          play (Some new_c) (change_current_players_hand c new_c gst)
        with
        | Legal next ->
            buffer next_gst;
            game_loop next
        | _ -> print_string "Illegal game state.\n"
      else buffer next_gst;
      game_loop next_gst

let play_game players =
  let start_state = init_state standard_cards players in
  game_loop start_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nUno \nType 'Quit' at any time to quit the game.\n\n";
  let rec getPlayers u =
    let restart () =
      clear ();
      print_string fail_str;
      getPlayers ()
    in
    print_endline
      "Type in the number of people that want to play the game.\n";
    print_string "> ";
    match check_quit () with
    | None -> restart ()
    | Some n -> (
        print_endline
          "How many computer opponents do you want? (In addition to \
           the earlier number).\n";
        print_string "> ";
        match check_quit () with
        | None -> restart ()
        | Some c ->
            if n + c >= 2 && n > 0 && c >= 0 then
              c |> create_players [] n |> play_game
            else restart ())
  in
  getPlayers ()

(* Execute the game engine. *)
let () = main ()
