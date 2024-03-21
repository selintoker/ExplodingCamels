type card_type =
  | ExplodingCamel
  | Water
  | Resource of string

type card = {
  card_type : card_type; (*distinguishes what type of card it is*)
  card_id : int; (* Each card will have a unique ID for identification *)
  functionality : string;
      (*contains a description of the functionality of the card*)
  deck : bool;
}

(** Given the card type and card id, we create a new card within the deck *)
let make card_type card_id =
  { card_type; card_id; functionality = ""; deck = true }

(** Given the variable type card, this function prints out the name of the card*)
let name card = card.card_id

(** Given the variable type card, this function returns the functionality of the
    card*)
let functionality card = card.functionality

(** Given the variable type card, this function returns whether the card is
    inside the deck or not *)
let deck card = card.deck

(** Given the variable type card, this function returns the type of the card*)
let return_card_type card = card.card_type

(** Function to split a list at a specific index *)
let rec split_at index lst =
  match (index, lst) with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let left, right = split_at (n - 1) xs in
      (x :: left, right)

(** Given the deck type 'a list, this function shuffles a list of elements by
    randomizing the order of the element and return the 'a list *)
let shuffle_deck deck =
  let rec insert_random element acc =
    let len = List.length acc in
    let random_index = Random.int (len + 1) in
    let left, right = split_at random_index acc in
    left @ [ element ] @ right
  in
  let shuffled_deck =
    List.fold_left (fun acc card -> insert_random card acc) [] deck
  in
  shuffled_deck

(** Given the number of players within the game integer [n], this function
    creates a new deck of cards. It produces n - 1 cards of exploding camel and
    create more than n cards of water, which will serve as difuse in this game.
    Then, with the given deck, the function shuffles the deck of cards. *)

(**CATCH BUG HERE? -- possible err out location 1*)
let init_deck num_players =
  let exploding_camels =
    List.init (num_players - 1) (fun i -> make ExplodingCamel (i + 1))
  in
  let water_cards = List.init 3 (fun i -> make Water (i + 1)) in
  (* Create resource cards with unique names *)
  let resource_cards =
    List.flatten
      (List.init 5 (fun _ ->
           let resource_names =
             [
               "Shuffle";
               "See the Future";
               "Draw From Bottom";
               "Alter the Future";
               "Skip";
               "Bury";
               "Steal";
               "Draw";
             ]
           in
           List.init (List.length resource_names) (fun i ->
               make (Resource (List.nth resource_names i)) (i + num_players + 6))))
  in

  shuffle_deck (water_cards @ exploding_camels @ resource_cards)

(** Given the card of the variable, the function returns the type of the card's
    type. *)
let string_of_card card =
  match card.card_type with
  | Water -> "Water"
  | ExplodingCamel -> "Exploding Camel"
  | Resource res -> res

(** Given the deck of the cards [card list], the function allows the user to see
    the first three cards of the deck from the top. *)
let see_the_future deck =
  let rec aux n deck card_names =
    match (n, deck) with
    | 0, _ -> card_names
    | _, [] -> card_names
    | n, card :: rest_deck ->
        let card_name = string_of_card card in
        aux (n - 1) rest_deck (card_name :: card_names)
  in
  let top_cards = aux 3 deck [] in
  "Top 3 cards: " ^ String.concat ", " (List.rev top_cards)

let draw_from_bottom deck =
  match deck with
  | [] -> failwith "Empty deck"
  | top_card :: rest -> (
      let reversed_deck = List.rev deck in
      match reversed_deck with
      | [] -> failwith "Unexpected empty reversed deck"
      | new_top_card :: remaining ->
          let new_deck = new_top_card :: List.rev remaining in
          new_deck)

(* Function to see the top 3 cards and alter their order based on user input *)
let alter_the_future deck =
  let rec aux n deck card_names =
    match (n, deck) with
    | 0, _ -> card_names
    | _, [] -> card_names
    | n, card :: rest_deck ->
        let card_name = card in
        aux (n - 1) rest_deck (card_name :: card_names)
  in
  let top_cards = List.rev (aux 3 deck []) in

  (* Display top cards with numbers to the user *)
  let top_cards_with_numbers =
    List.mapi
      (fun i card -> Printf.sprintf "%d- %s" (i + 1) (string_of_card card))
      top_cards
  in
  let top_cards_str =
    "Top 3 cards:\n" ^ String.concat "\n" top_cards_with_numbers
  in
  print_endline top_cards_str;

  (* Ask the user to specify the order of the top 3 cards *)
  let prompt_user_to_order_card position =
    Printf.sprintf "Which of these cards should be %s? Enter the number: "
      position
  in

  let user_order =
    List.map
      (fun prompt ->
        print_string (prompt_user_to_order_card prompt);
        read_int ())
      [ "at the top"; "the second from the top"; "the third from the top" ]
  in

  (* Reorder the top cards based on user input *)
  let reordered_cards =
    List.map (fun position -> List.nth top_cards (position - 1)) user_order
  in

  (* Create the altered deck *)
  let altered_deck =
    reordered_cards
    @ List.filter (fun card -> not (List.mem card top_cards)) deck
  in

  altered_deck

(* Function to bury the top card of the deck *)
let bury deck =
  match deck with
  | [] ->
      print_endline "The deck is empty.";
      deck
  | top_card :: rest_of_deck ->
      print_endline ("Top card: " ^ string_of_card top_card);

      print_endline
        "Enter the position where you want to bury the top card (1 for the \
         top, 2 for the second, etc.): ";
      let bury_position = read_int () in

      let buried_deck =
        let before, after = split_at (bury_position - 1) rest_of_deck in
        before @ (top_card :: after)
      in

      print_endline "Top card buried.";
      buried_deck

let bury_test deck where =
  match deck with
  | [] -> deck
  | top_card :: rest_of_deck ->
      let bury_position = where in
      let buried_deck =
        let before, after = split_at (bury_position - 1) rest_of_deck in
        before @ (top_card :: after)
      in
      buried_deck

let print_deck deck =
  List.iter
    (fun card ->
      Printf.printf "%s (ID: %d)\n" (string_of_card card) card.card_id)
    deck

let deck_length deck = List.length deck

let print_deck_length deck =
  let length = deck_length deck in
  Printf.printf "Length of the deck: %d\n" length