open Cards
open Player

let rec read_valid_int () =
  try read_int ()
  with Failure _ ->
    Printf.printf "Invalid input. Please enter a valid number: ";
    read_valid_int ()

let create_players_test num_players =
  List.init num_players (fun i ->
      create ("Player " ^ string_of_int (i + 1)) [] 0)

let pop_deck_test (deck : card list) : 'a list =
  match deck with
  | [] -> []
  | card :: rest -> rest

type draw_result_test =
  | NoCard
  | ExplodingCamel
  | AnotherCard of player

let draw_card_test (player : player) (deck : card list) : draw_result_test =
  match deck with
  | [] -> NoCard
  | card :: rest -> (
      Printf.printf"\n
      \n 
      ";
      Printf.printf "%s drew a card: " player.name;
      let updated_player = add_card player card in
      match return_card_type card with
      | ExplodingCamel ->
          Printf.printf "Exploding Camel!\n";
          ExplodingCamel
      | _ ->
          Printf.printf "Another card.\n";
          AnotherCard updated_player)

let create_players num_players =
  List.init num_players (fun i ->
      create ("Player " ^ string_of_int (i + 1)) [] 0)

let pop_deck (deck : card list) : 'a list =
  match deck with
  | [] -> []
  | card :: rest -> rest

let remove_at index lst =
  let rec remove_at_helper i acc = function
    | [] -> List.rev acc
    | x :: rest ->
        if i = 0 then List.rev_append (List.rev acc) rest
        else remove_at_helper (i - 1) (x :: acc) rest
  in
  remove_at_helper index [] lst

let update_hand_after_play (player : player) (index : int) : card list =
  remove_at (index - 1) (get_hand player)

let rec get_valid_target_player players =
  Printf.printf "Choose a player to steal from (1 to %d): "
    (List.length players);
  let target_player_index = read_valid_int () in
  match List.nth_opt players (target_player_index - 1) with
  | Some target_player -> target_player
  | None ->
      Printf.printf "Invalid target player index. Please try again.\n";
      get_valid_target_player players

let play_see_the_future deck player index =
  Printf.printf "See the Future card played.\n
  \n
  \n 
  ";
  Printf.printf "%s\n" (see_the_future deck);
  let updated_deck = pop_deck deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, false)

let play_alter_the_future deck player index =
  Printf.printf "Alter the Future card played.\n
  \n
  \n 
 ";
  let updated_deck = alter_the_future deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, false)

let play_shuffle deck player index =
  Printf.printf "Shuffle card played.\n
  \n
  \n 
  \n
  \n
  \n
  \n
  \n 
  \n
  \n
  \n
  \n
  \n 
  \n
  \n
  \n
  \n";
  let updated_deck = shuffle_deck deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, false)

let play_draw_from_bottom deck player index =
  Printf.printf "Draw From Bottom card played.\n
  \n
  \n 
  ";
  let updated_deck = draw_from_bottom deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, false)

let play_skip deck player index =
  Printf.printf "Skip card played.\n
  \n
  \n 
  ";
  let updated_deck = deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, true)

let play_bury deck player index =
  Printf.printf "Bury card played.\n
  \n 
  \n
  ";
  let updated_deck = bury deck in
  let updated_hand = update_hand_after_play player index in
  let updated_player = { player with hand = updated_hand } in
  ([ updated_player ], updated_deck, false)

let play_steal players deck player index =
  Printf.printf "Steal card played.\n
  \n
  \n 
 ";
  let target_player = get_valid_target_player players in
  let target_hand = get_hand target_player in

  match target_hand with
  | [] ->
      Printf.printf "The target player has no cards to steal.\n
      ";
      let updated_hand = remove_at (index - 1) (get_hand player) in
      let updated_player = { player with hand = updated_hand } in
      ([ updated_player ], deck, false)
  | _ ->
      let random_index = Random.int (List.length target_hand) in
      let stolen_card = List.nth target_hand random_index in
      Printf.printf "%s stole a card from %s!\n
      \n
  \n 
  \n
 " player.name target_player.name;

      let updated_hand = update_hand_after_play player index in
      let updated_player = { player with hand = stolen_card :: updated_hand } in
      ([ updated_player ], deck, false)

let play_draw card deck player index =
  Printf.printf "Draw card played.\n
  \n
  \n 
  ";
  match deck with
  | [] ->
      Printf.printf "Deck is empty. No card drawn.\n
      \n
  \n 
  \n";
      ([ player ], deck, false)
  | drawn_card :: updated_deck ->
      let updated_hand = update_hand_after_play player index in
      let updated_player = { player with hand = drawn_card :: updated_hand } in
      ([ updated_player ], updated_deck, false)
