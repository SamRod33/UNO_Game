open Card

exception CardNotInHand of (Card.t list)

type t = {
  name : string;
  hand : Card.t list
}

let name = t.name

let player_hand = t.hand

let is_uno cards = 
  match cards with
  | [] -> true
  | h :: [] -> true
  | _ -> false

let add_card player card = 
  {player with hand = card::hand}

let remove_card player card = 
  let new_hand = (List.filter (fun x -> (x = card)) player.hand) in
  if hand = new_hand then raise CardNotInHand
  else {player with hand = new_hand}