open State
open Card
open Player

let rec create_players players = function
  | 0 -> players
  | n ->
      let player = create ("Player " ^ string_of_int n) false in
      create_players (player :: players) (n - 1)

let cards_str cards =
  List.fold_left (fun str card -> str ^ "[card printed here] ") "" cards

let fail_str = "\nTry again.\n"

let illegal_card = "\nYou can't play that card. Try another.\n"

let play_game players =
  let start_state = init_state standard_cards players in
  let rec game_loop g =
    let cur_player = current_player g in
    let cur_player_hand = player_hand cur_player in
    print_string ("It is " ^ name cur_player ^ "'s turn.\n");
    print_string
      ("The top card is " ^ "[card] "
     ^ "and the current stack penalty is "
      ^ string_of_int (stack_penalty g)
      ^ ".\n");
    print_string ("Your cards are: " ^ cards_str cur_player_hand ^ "\n");
    print_endline
      "Type in the index of the card you wish to play (starting from \
       0). If you do not have a card to play, type -1.\n";
    print_string "> ";
    match int_of_string_opt (read_line ()) with
    | None ->
        print_string fail_str;
        game_loop g
    | Some n ->
        if n > -2 && n < List.length (player_hand cur_player) then (
          let play_card =
            if n = -1 then None else Some (List.nth cur_player_hand n)
          in
          match play play_card g with
          | Illegal ->
              print_string illegal_card;
              game_loop g
          | Legal next_g -> game_loop next_g
          | GameOver winner ->
              print_string (name winner ^ "wins!");
              exit 0)
        else print_string fail_str;
        game_loop g
  in
  game_loop start_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Uno. \n";
  let rec getPlayers u =
    print_endline
      "Type in the number of people that want to play the game.\n";
    print_string "> ";
    match int_of_string_opt (read_line ()) with
    | None ->
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
