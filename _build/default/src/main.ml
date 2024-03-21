open Lib.Cards
open Lib.Player
open Lib.Gameplay
open Bogue
open Tsdl
module W = Widget
module L = Layout
module T = Trigger

let section_title s = L.flat_of_w [ W.label ~size:12 ~fg:Draw.(opaque grey) s ]

let hline width =
  let style = Style.(empty |> with_bg (color_bg Draw.(transp black))) in
  L.resident (W.box ~w:width ~h:1 ~style ())

let empty_window num_players =
  let width = 400 in

  let quit_title = section_title "Exploding Camels" in
  let quit_btn = W.button ~border_radius:10 "QUIT" in
  let yes_action () = T.push_quit () in
  let no_action () = () in

  let quit_layout =
    L.tower ~margins:0 ~align:Draw.Center [ quit_title; L.resident quit_btn ]
  in
  let bottom = L.flat ~align:Draw.Max ~margins:0 [ quit_layout ] in
  L.set_width bottom width;

  let tabs_list =
    List.init num_players (fun i ->
        let page = L.tower [ hline width; bottom ] in
        ("Page " ^ string_of_int (i + 1), page))
  in
  let tabs = Tabs.create ~slide:Avar.Right tabs_list in

  let release _ =
    Popup.yesno ~w:100 ~h:50 "Really quit?" ~yes_action ~no_action tabs
  in
  W.on_button_release ~release quit_btn;

  let board = Main.make [] [ tabs ] in
  Main.run board

let draw_card player deck =
  if player.status = 0 then ([], deck)
  else
    match deck with
    | [] ->
        (* The player's turn ends, return an empty list of players *)
        ([], [])
    | card :: rest -> (
        Printf.printf "%s drew a card: " player.name;
        let updated_player = add_card player card in
        match return_card_type card with
        (* if there are no water card within the hand, then the player is out of
           the game *)
        | ExplodingCamel ->
            let has_water_card =
              List.exists
                (fun c -> return_card_type c = Water)
                (get_hand player)
            in
            if has_water_card then (
              Printf.printf
                "Exploding Camel! You have a Water card to defuse it.\n";
              let updated_hand =
                List.filter
                  (fun c -> return_card_type c <> Water)
                  (get_hand player)
              in
              Printf.printf
                "Enter the position (1 to %d) to place the Exploding Camel \
                 card: "
                (List.length deck + 1);
              let position = read_valid_int () in
              let updated_deck =
                let left, right = split_at (position - 1) rest in
                left @ (card :: right)
              in
              Printf.printf
                "Exploding Camel defused and placed back into the deck.\n";
              ([ { player with hand = updated_hand } ], updated_deck))
            else (
              Printf.printf "Exploding Camel! They are out of the game!\n";
              ([ { player with status = 0 } ], rest)
              (* Player is out of the game, remove them *))
        | _ ->
            Printf.printf "Safe card drawn!\n";
            ([ updated_player ], pop_deck deck))

let play_card players player index deck =
  if index >= 1 && index <= List.length (get_hand player) then (
    let card = List.nth (get_hand player) (index - 1) in
    Printf.printf "%s played a card: %s\n" player.name (string_of_card card);

    match return_card_type card with
    | Resource "See the Future" -> play_see_the_future deck player index
    | Resource "Shuffle" -> play_shuffle deck player index
    | Resource "Draw From Bottom" -> play_draw_from_bottom deck player index
    | Resource "Skip" -> play_skip deck player index
    | Resource "Bury" -> play_bury deck player index
    | Resource "Steal" -> play_steal players deck player index
    | Resource "Draw" -> play_draw card deck player index
    | Resource "Alter the Future" -> play_alter_the_future deck player index
    | _ ->
        Printf.printf "Invalid card played. No special action.\n";
        let updated_deck = pop_deck deck in
        let updated_hand = remove_at (index - 1) (get_hand player) in
        let updated_player = { player with hand = updated_hand } in
        ([ updated_player ], updated_deck, false))
  else begin
    Printf.printf "Invalid card index. Please try again.\n";
    ([ player ], deck, false)
  end

let rec player_interface players player rest_players deck =
  let width = 400 in
  let quit_title = section_title (player.name ^ "'s turn") in
  let hand_label =
    W.label (String.concat ", " (List.map string_of_card (get_hand player)))
  in
  let draw_btn = W.button ~border_radius:10 "Draw a card" in
  let play_btn = W.button ~border_radius:10 "Play a card" in
  let quit_btn = W.button ~border_radius:10 "Quit?" in
  let input_entry = W.text_input ~prompt:"Enter card index" () in
  let message_label = W.text_display ~w:200 ~h:100 "Take your turn!" in

  let draw_action () =
    W.set_text message_label "Drew a card.";
    let updated_players, updated_deck = draw_card player deck in
    match updated_players with
    | [] ->
        W.set_text hand_label
          (String.concat ", " (List.map string_of_card updated_deck));
        game_loop_gui updated_players updated_deck
    | _ ->
        W.set_text hand_label
          (String.concat ", " (List.map string_of_card updated_deck));
        game_loop_gui (rest_players @ updated_players) updated_deck
  in

  let play_action () =
    if List.length (get_hand player) > 0 then begin
      let card_index_str = W.get_text input_entry in
      match int_of_string_opt card_index_str with
      | Some card_index
        when card_index >= 1 && card_index <= List.length (get_hand player) ->
          W.set_text message_label "Played a card.";
          let updated_players, updated_deck, skip_next_turn =
            play_card players player card_index deck
          in
          if skip_next_turn then begin
            W.set_text message_label "Skipped next turn.";
            game_loop_gui updated_players updated_deck
          end
          else game_loop_gui (updated_players @ rest_players) updated_deck
      | _ -> W.set_text message_label "Invalid card index. Please try again."
    end
    else begin
      W.set_text message_label "%s has no cards to play.\n";
      game_loop_gui rest_players deck
    end
  in

  let layout =
    L.tower ~margins:0 ~align:Draw.Center
      [
        quit_title;
        L.resident hand_label;
        L.resident message_label;
        L.flat_of_w [ draw_btn; play_btn ];
        L.resident input_entry;
        L.resident quit_btn;
      ]
  in
  L.set_width layout width;
  let window = Main.of_layout layout in
  W.on_button_release ~release:(fun _ -> draw_action ()) draw_btn;
  W.on_button_release ~release:(fun _ -> play_action ()) play_btn;
  W.on_button_release ~release:(fun _ -> Bogue.quit ()) quit_btn;

  Main.run window
(* and handle_turn_result updated_players updated_deck = match updated_players
   with | [] -> print_deck updated_deck; game_loop updated_players updated_deck
   (* Player eliminated *) | _ -> print_deck updated_deck; game_loop
   updated_players updated_deck (* Continue with updated players and deck *) *)

and game_loop_gui players deck =
  match players with
  | [] -> ()
  | player :: rest_players ->
      player_interface players player rest_players deck;
      game_loop_gui rest_players deck

let rec game_loop_terminal players deck =
  match players with
  | [] -> ()
  | player :: [] ->
      Printf.printf "%s is the winner!\n" player.name;
      exit 0
  | player :: rest_players ->
      if count_active_players players = 1 then begin
        Printf.printf "%s is the winner!\n" player.name;
        exit 0
      end;
      if player.status = 0 then game_loop_terminal rest_players deck
      else (
        Printf.printf "%s's turn.\n" player.name;
        Printf.printf "Hand: %d cards\n" (List.length (get_hand player));
        Printf.printf "Hand: %s\n"
          (String.concat ", " (List.map string_of_card (get_hand player)));
        (* Print the player's hand *)
        Printf.printf "Options:\n";
        Printf.printf "1. Draw a card and end turn\n";
        Printf.printf "2. Play a card and continue\n";
        Printf.printf "Enter your choice: ";
        let choice = read_valid_int () in
        match choice with
        | 1 -> (
            let updated_players, updated_deck = draw_card player deck in
            match updated_players with
            | [] ->
                print_deck_length updated_deck;
                game_loop_terminal updated_players updated_deck
                (* Player eliminated *)
            | _ ->
                print_deck_length updated_deck;
                game_loop_terminal (rest_players @ updated_players) updated_deck
            (* Continue with updated players and deck *))
        | 2 ->
            if List.length (get_hand player) > 0 then begin
              Printf.printf
                "Enter the number of the card you want to play (1 to %d): "
                (List.length (get_hand player));
              let card_index = read_valid_int () in
              if card_index >= 1 && card_index <= List.length (get_hand player)
              then
                let updated_players, updated_deck, skip_next_turn =
                  play_card players player card_index deck
                  (* Pass the index, not the card *)
                in
                if skip_next_turn then begin
                  Printf.printf "Skipped next turn.\n";
                  game_loop_terminal
                    (rest_players @ updated_players)
                    updated_deck
                end
                else
                  game_loop_terminal
                    (updated_players @ rest_players)
                    updated_deck
              else Printf.printf "Invalid card index. Please try again.\n";
              game_loop_terminal players deck
            end
            else begin
              Printf.printf "%s has no cards to play.\n" player.name;
              game_loop_terminal players deck
            end
        | _ ->
            Printf.printf "Invalid choice. Please try again.\n";
            game_loop_terminal players deck)

let rec choose_interface () =
  Printf.printf "Choose interface (1: GUI, 2: Terminal): ";
  let choice = read_valid_int () in
  match choice with
  | 1 -> true (* Use GUI *)
  | 2 -> false (* Use Terminal *)
  | _ ->
      Printf.printf "Invalid choice. Please enter 1 or 2.\n";
      choose_interface ()

let print_instructions () =
  Printf.printf
    "Welcome to Exploding Camels!\n\n\
     Instructions:\n\
     - Players take turns laying cards during their turn.\n\
     - You can play any number of cards before drawing a card to end your turn.\n\
     - If you draw an Exploding Camel, you need a Water card to defuse it.\n\
     - Without a Water card, you are out of the game.\n\
     - Defused Exploding Camels can be placed back in the deck.\n\
     - The winner is the last player left after everyone else's camels explode.\n\
     Special Cards:\n\
     - See the Future: View the top three cards of the deck.\n\
     - Shuffle: Shuffle the deck.\n\
     - Draw From Bottom: Draw a card from the bottom of the deck.\n\
     - Skip: Skip the next player's turn.\n\
     - Steal: Take a card from another player's hand.\n\
     - Alter the Future: Rearrange the top three cards of the deck.\n\
     - Draw: Draw the top card from the deck without ending your turn.\n\n\
     - Bury: Draw the top card from the deck and decide to place where to put \
     the card back in. \n\
    \     Let the game begin!\n\n"

let () =
  print_instructions ();
  Printf.printf "Enter the number of players: ";
  let num_players = read_valid_int () in
  let use_gui = choose_interface () in
  let players = create_players num_players in
  let initial_deck = shuffle_deck (init_deck num_players) in
  if use_gui then (
    game_loop_gui players initial_deck;
    empty_window num_players)
  else game_loop_terminal players initial_deck
