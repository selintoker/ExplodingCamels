open Cards

type player = {
  name : string;
  hand : card list;
  status : int;
}

(** Given the name of the string string, hand [card list], and status [integer],
    each player will given 7 cards including at least one water card. It creates
    a player based on the record type of the player *)
(* let create (name : string) (hand : card list) (status : int) : player =
   Random.self_init (); let hand = let num_cards = 7 in let num_water = 1 in let
   num_shuffle = Random.int (num_cards - num_water + 1) in let
   num_see_the_future = Random.int (num_cards - num_water - num_shuffle + 1) in
   let num_draw_from_bottom = num_cards - num_water - num_shuffle -
   num_see_the_future in let shuffle_cards = List.init num_shuffle (fun _ ->
   make (Resource "Shuffle") 0) in let see_the_future_cards = List.init
   num_see_the_future (fun _ -> make (Resource "See the Future") 0) in let
   draw_from_bottom_cards = List.init num_draw_from_bottom (fun _ -> make
   (Resource "Draw From Bottom") 0) in let water_card = make Water 1 in
   water_card :: (shuffle_cards @ see_the_future_cards @ draw_from_bottom_cards)
   in { name; hand; status } *)

let create (name : string) (hand : card list) (status : int) : player =
  Random.self_init ();

  (* List of possible resource cards *)
  let resource_cards =
    [
      Resource "Shuffle";
      Resource "See the Future";
      Resource "Draw From Bottom";
      Resource "Skip";
      Resource "Alter the Future";
      Resource "Bury";
      Resource "Steal";
      Resource "Draw";
    ]
  in

  let choose_random_card () =
    List.nth resource_cards (Random.int (List.length resource_cards))
  in

  let rec add_random_resource_cards num_cards acc =
    if num_cards <= 0 then acc
    else begin
      let random_card = make (choose_random_card ()) 0 in
      add_random_resource_cards (num_cards - 1) (random_card :: acc)
    end
  in

  let water_card = make Water 1 in

  let additional_resource_cards = add_random_resource_cards 6 [] in

  let hand = water_card :: additional_resource_cards in

  let status = 1 in

  { name; hand; status }

(** Given the variable player [player] and card [card], the function adds a card
    to the player's hand *)
let add_card (player : player) (card : card) : player =
  { player with hand = card :: player.hand }

(** Given the variable [player] and card [card], the function removes a specific
    card from the player's hand, only if the card exists in its hand *)
let remove_card (player : player) (card : card) : player =
  let updated_hand = List.filter (fun c -> c <> card) player.hand in
  { player with hand = updated_hand }

(** Given the variable type [player] and the type [card], the function returns
    whether the player's hand contains the input specific card*)
let has_card (player : player) (card : card) : bool =
  List.exists (fun c -> c = card) player.hand

(** Given the variable type [player], the function returns the player's hand as
    a list of cards *)
let get_hand (player : player) : card list = player.hand

let get_hand_length (player : player) : int = List.length player.hand

(** Given the variable type [player], the function returns the player's name *)
let get_name (player : player) : string = player.name

let empty_hand (player : player) : player = { player with hand = [] }

let set_hand (player : player) (new_hand : card list) : player =
  { player with hand = new_hand }

let count_active_players players =
  List.fold_left
    (fun count player -> if player.status = 1 then count + 1 else count)
    0 players
