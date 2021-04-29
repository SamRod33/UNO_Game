open State
open Card
open Player

(** [create_players \[\] n] is a list of [n] players.*)
let rec create_players players = function
  | 0 -> players
  | n ->
      let player = create ("Player " ^ string_of_int n) false in
      create_players (player :: players) (n - 1)

let check_quit () =
  match read_line () with
  | "Quit" ->
      print_string "Thanks for playing!\n\n";
      exit 0
  | a -> int_of_string_opt a

let fail_str = "\nTry again.\n"

let illegal_card = "\nYou can't play that card. Try another.\n"

let rec select_color () =
  print_string "\n\nType in R, G, B, or Y to select the color.\n\n";
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

let play_game players =
  let start_state = init_state standard_cards players in
  let rec game_loop g =
    let format_card gst next_gst = function
      | None -> game_loop next_gst
      | Some c ->
          if color c = ANY then
            let new_c = change_color c (select_color ()) in
            match
              play (Some new_c)
                (change_current_players_hand c new_c gst)
            with
            | Legal next -> game_loop next
            | _ -> print_string "Illegal game state.\n"
          else game_loop next_gst
    in
    let cur_player = current_player g in
    let cur_player_hand = player_hand cur_player in
    print_string ("It is " ^ name cur_player ^ "'s turn.\n");
    print_string "The top card is:\n\n";
    pp_cards [ top_card g ] false;
    print_string
      ("\nThe current stack penalty is "
      ^ string_of_int (stack_penalty g)
      ^ ".\n");
    print_string "Your cards are:\n\n";
    pp_cards cur_player_hand true;
    print_endline
      "\n\
       Type in the index of the card you wish to play (starting from \
       0). If you do not have a card to play, type -1.\n";
    print_string "> ";
    match check_quit () with
    | None ->
        ignore (Sys.command "clear");
        print_string fail_str;
        game_loop g
    | Some n ->
        if n > -2 && n < List.length cur_player_hand then (
          let play_card =
            if n = -1 then None else Some (List.nth cur_player_hand n)
          in
          match play play_card g with
          | Illegal ->
              ignore (Sys.command "clear");
              print_string illegal_card;
              game_loop g
          | Legal next_g ->
              ignore (Sys.command "clear");
              print_string
                ("It is "
                ^ name (current_player next_g)
                ^ "'s turn. Enter anything to continue.\n");
              read_line ();
              ignore (Sys.command "clear");
              format_card g next_g play_card
          | GameOver winner ->
              print_string ("\n" ^ name winner ^ " wins!\n\n");
              exit 0)
        else ignore (Sys.command "clear");
        print_string fail_str;
        game_loop g
  in
  game_loop start_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to Uno. \n\
     Type 'Quit' at any time to quit the game.\n\n";
  let rec getPlayers u =
    print_endline
      "Type in the number of people that want to play the game.\n";
    print_string "> ";
    match check_quit () with
    | None ->
        ignore (Sys.command "clear");
        print_string fail_str;
        getPlayers ()
    | Some n ->
        if n >= 2 then n |> create_players [] |> play_game
        else (
          print_string fail_str;
          getPlayers ())
  in
  getPlayers ()

(* Execute the game engine. *)
let () = main ()
