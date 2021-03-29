open Card

exception CardNotInHand of Card.t

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

(** [remove_first_of_dup hand card acc] is the player's hand after removing the
    first of any given card form the player's original hand [hand] if the card
    [card] is in the hand. The accumulator [acc] stores the cards checked and
    unequal to the card to be removed. *)
let rec remove_first_of_dup hand card (acc : Card.t list) = 
  match hand with
  | [] -> hand
  | h :: t -> if h = card then acc :: t else remove_first_of_dup t card (h::acc)

let remove_card player card = 
  if not (List.mem card player.hand) then raise (CardNotInHand card)
  else let new_hand = remove_first_of_dup player.hand card [] in
  {player with hand = new_hand}
