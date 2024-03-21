(** Test Plan:

    We have utilized a combination of OUnit tests and manual testing to ensure
    the correctness of our system. The OUnit tests cover key functionalities
    within the Cards, Player, and Gameplay modules. We applied both black-box
    and glass-box testing, evaluating the system's behavior with both expected
    and unexpected inputs.

    OUnit Tests:
    - Cards module: We tested the functions related to card creation,
      manipulation, and deck operations. This includes functions such as [make],
      [shuffle_deck], [init_deck], and functionalities of each card types.
    - Player module: The tests cover player creation, hand management, and
      status updates. Functions like [create], [add_card], and [remove_card]
      were tested.
    - Gameplay module: We tested various gameplay functions, such as drawing
      cards, player actions like stealing or skipping turns, and updating game
      status. Evaluated the correctness of interactions between players and the
      deck, ensuring valid outcomes based on card plays.

    Manual Testing:
    - We performed manual testing during the development process to ensure the
      overall integration of modules and the multiplayer experience.
    - Edge cases and unexpected scenarios were tested, including handling empty
      decks and unusual player actions, verifying the system's behavior under
      different conditions.
    - The graphical user interface (GUI) functionalities were manually tested
      through execution, focusing on general usage and interaction without a
      detailed breakdown of specific test cases.

    The testing approach demonstrates the correctness of the system by
    systematically verifying individual module functionalities through OUnit
    tests and ensuring the overall system's integrity through manual testing. *)

open OUnit2
open Lib
open Player
open Cards
open Gameplay
open Game_status

(********get hand length ************)
let test_get_hand_length_1 () =
  let player = create "Player1" [] 0 in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_2 () =
  let player =
    create "Player2" [ make Water 1; make (Resource "Shuffle") 0 ] 1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_3 () =
  let player =
    create "Player3"
      [
        make Water 1;
        make (Resource "Draw") 0;
        make (Resource "See the Future") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_4 () =
  let player =
    create "Player4"
      [
        make (Resource "Steal") 0;
        make (Resource "Skip") 0;
        make (Resource "Bury") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_5 () =
  let player =
    create "Player5"
      [
        make (Resource "Alter the Future") 0;
        make (Resource "Draw From Bottom") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_6 () =
  let player =
    create "Player6"
      [
        make (Resource "Draw") 0;
        make (Resource "See the Future") 0;
        make (Resource "Shuffle") 0;
        make Water 1;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_7 () =
  let player =
    create "Player7"
      [
        make (Resource "Skip") 0;
        make (Resource "Bury") 0;
        make (Resource "Steal") 0;
        make Water 1;
        make (Resource "Draw From Bottom") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_8 () =
  let player =
    create "Player8"
      [
        make (Resource "Bury") 0;
        make (Resource "Steal") 0;
        make (Resource "Draw From Bottom") 0;
        make (Resource "Draw") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_9 () =
  let player =
    create "Player9"
      [
        make (Resource "Alter the Future") 0;
        make (Resource "Draw From Bottom") 0;
        make (Resource "Skip") 0;
        make (Resource "See the Future") 0;
        make Water 1;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let test_get_hand_length_10 () =
  let player =
    create "Player10"
      [
        make (Resource "Draw From Bottom") 0;
        make (Resource "Shuffle") 0;
        make (Resource "Steal") 0;
        make (Resource "Draw") 0;
      ]
      1
  in
  assert_equal (get_hand_length player) 7

let get_hand_length_tests =
  [
    ("get_hand_length test 1" >:: fun _ -> test_get_hand_length_1 ());
    ("get_hand_length test 2" >:: fun _ -> test_get_hand_length_2 ());
    ("get_hand_length test 3" >:: fun _ -> test_get_hand_length_3 ());
    ("get_hand_length test 4" >:: fun _ -> test_get_hand_length_4 ());
    ("get_hand_length test 5" >:: fun _ -> test_get_hand_length_5 ());
    ("get_hand_length test 6" >:: fun _ -> test_get_hand_length_6 ());
    ("get_hand_length test 7" >:: fun _ -> test_get_hand_length_7 ());
    ("get_hand_length test 8" >:: fun _ -> test_get_hand_length_8 ());
    ("get_hand_length test 9" >:: fun _ -> test_get_hand_length_9 ());
    ("get_hand_length test 10" >:: fun _ -> test_get_hand_length_10 ());
  ]

(***************************SET HAND _****************************)

let test_set_hand_1 () =
  let player = create "Player1" [] 0 in
  let new_hand =
    [
      make Water 1;
      make (Resource "Shuffle") 0;
      make (Resource "See the Future") 0;
    ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 3

let test_set_hand_2 () =
  let player = create "Player2" [ make Water 1 ] 1 in
  let new_hand =
    [ make (Resource "Draw From Bottom") 0; make (Resource "Skip") 0 ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 2

let test_set_hand_3 () =
  let player =
    create "Player3"
      [
        make (Resource "Bury") 0;
        make (Resource "Steal") 0;
        make (Resource "Alter the Future") 0;
      ]
      1
  in
  let new_hand =
    [
      make (Resource "Draw") 0; make (Resource "See the Future") 0; make Water 1;
    ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 3

let test_set_hand_4 () =
  let player =
    create "Player4"
      [ make (Resource "Shuffle") 0; make (Resource "Draw From Bottom") 0 ]
      1
  in
  let new_hand = [] in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 0

let test_set_hand_5 () =
  let player =
    create "Player5" [ make (Resource "Draw") 0; make (Resource "Bury") 0 ] 1
  in
  let new_hand = [ make (Resource "Steal") 0 ] in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 1

let test_set_hand_6 () =
  let player =
    create "Player6"
      [ make Water 1; make (Resource "Shuffle") 0; make (Resource "Draw") 0 ]
      1
  in
  let new_hand =
    [ make (Resource "See the Future") 0; make (Resource "Skip") 0 ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 2

let test_set_hand_7 () =
  let player =
    create "Player7"
      [ make (Resource "Draw From Bottom") 0; make (Resource "Skip") 0 ]
      1
  in
  let new_hand =
    [
      make (Resource "Alter the Future") 0;
      make Water 1;
      make (Resource "Steal") 0;
    ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 3

let test_set_hand_8 () =
  let player =
    create "Player8"
      [
        make (Resource "See the Future") 0;
        make (Resource "Bury") 0;
        make (Resource "Steal") 0;
      ]
      1
  in
  let new_hand = [ make Water 1; make (Resource "Shuffle") 0 ] in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 2

let test_set_hand_9 () =
  let player =
    create "Player9"
      [ make (Resource "Skip") 0; make (Resource "Alter the Future") 0 ]
      1
  in
  let new_hand =
    [
      make (Resource "Draw From Bottom") 0;
      make Water 1;
      make (Resource "Draw") 0;
    ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 3

let test_set_hand_10 () =
  let player =
    create "Player10"
      [ make Water 1; make (Resource "Steal") 0; make (Resource "Shuffle") 0 ]
      1
  in
  let new_hand = [ make (Resource "Bury") 0 ] in
  let updated_player = set_hand player new_hand in
  assert_equal (get_hand_length updated_player) 1

let set_hand_tests =
  [
    ("set_hand test 1" >:: fun _ -> test_set_hand_1 ());
    ("set_hand test 2" >:: fun _ -> test_set_hand_2 ());
    ("set_hand test 3" >:: fun _ -> test_set_hand_3 ());
    ("set_hand test 4" >:: fun _ -> test_set_hand_4 ());
    ("set_hand test 5" >:: fun _ -> test_set_hand_5 ());
    ("set_hand test 6" >:: fun _ -> test_set_hand_6 ());
    ("set_hand test 7" >:: fun _ -> test_set_hand_7 ());
    ("set_hand test 8" >:: fun _ -> test_set_hand_8 ());
    ("set_hand test 9" >:: fun _ -> test_set_hand_9 ());
    ("set_hand test 10" >:: fun _ -> test_set_hand_10 ());
  ]

(*************************** make_card tests ******************************)

let test_make () =
  let card = make Water 1 in
  let card2 = make ExplodingCamel 2 in
  assert (return_card_type card = Water);
  assert (return_card_type card2 = ExplodingCamel);
  assert (name card = 1);
  assert (name card2 = 2);
  assert (deck card = true);
  assert (deck card2 = true)

let test_make2 () =
  let card = make (Resource "Shuffle") 1 in
  let card2 = make ExplodingCamel 3 in
  assert (return_card_type card2 = ExplodingCamel);
  assert (name card = 1);
  assert (name card2 = 3);
  assert (deck card = true);
  assert (deck card2 = true)

let test_make3 () =
  let card = make (Resource "Alter the Future") 2 in
  let card2 = make Water 4 in
  assert (return_card_type card2 = Water);
  assert (name card = 2);
  assert (name card2 = 4);
  assert (deck card = true);
  assert (deck card2 = true)

let test_make4 () =
  let card = make (Resource "Bury") 5 in
  let card2 = make ExplodingCamel 1 in
  assert (return_card_type card2 = ExplodingCamel);
  assert (name card = 5);
  assert (name card2 = 1);
  assert (deck card = true);
  assert (deck card2 = true)

let make_tests =
  [
    ("Make test" >:: fun _ -> test_make ());
    ("Make test 2" >:: fun _ -> test_make2 ());
    ("Make test 3" >:: fun _ -> test_make3 ());
    ("Make test 4" >:: fun _ -> test_make4 ());
  ]

(*************************** shuffle_deck tests ******************************)
let cmp_deck_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

let test_deck = init_deck 4
let test_deck = init_deck 1
let shuffle_deck_test out in1 = assert_equal ~cmp:cmp_deck_like_lists out in1

let test_shuffle_deck_empty () =
  let shuffled_deck = shuffle_deck [] in
  assert_equal [] shuffled_deck

let test_shuffle_deck_single_card () =
  let card = make Water 1 in
  let shuffled_deck = shuffle_deck [ card ] in
  assert_equal [ card ] shuffled_deck

let test_shuffle_deck_large_deck () =
  let deck = init_deck 10 in
  let shuffled_deck = shuffle_deck deck in
  assert (cmp_deck_like_lists deck shuffled_deck)

let shuffle_deck_tests =
  [
    ( "shuffle test - 4" >:: fun _ ->
      shuffle_deck_test (shuffle_deck test_deck) test_deck );
    ( "shuffle test - 1" >:: fun _ ->
      shuffle_deck_test (shuffle_deck test_deck) test_deck );
    ( "shuffle test - 0" >:: fun _ ->
      shuffle_deck_test (shuffle_deck test_deck) test_deck );
    ("shuffle test - Empty Deck" >:: fun _ -> test_shuffle_deck_empty ());
    ("shuffle test - Single Card" >:: fun _ -> test_shuffle_deck_single_card ());
    ("shuffle test - Large Deck" >:: fun _ -> test_shuffle_deck_large_deck ());
  ]

(*************************** init_deck tests ******************************)

let init_deck_test num_players =
  let deck = init_deck num_players in
  let num_exploding_camels = num_players - 1 in
  let num_water_cards = 3 in
  let num_resource_cards = 40 in
  let expected_length =
    num_exploding_camels + num_water_cards + num_resource_cards
  in
  "init_deck test - " ^ string_of_int num_players >:: fun _ ->
  assert_equal expected_length (List.length deck)

let init_deck_tests =
  [
    init_deck_test 4;
    init_deck_test 1;
    init_deck_test 2;
    init_deck_test 5;
    init_deck_test 8;
  ]

(*************************** see_the_future tests******************************)

let see_the_future_test () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3 ] in
  let expected_output = "Top 3 cards: Water, Shuffle, Exploding Camel" in
  assert_equal expected_output (see_the_future deck)

let see_the_future_test2 () =
  let card1 = make (Resource "Favor") 1 in
  let card2 = make ExplodingCamel 1 in
  let card3 = make (Resource "Skip") 1 in
  let deck = [ card1; card2; card3 ] in
  let expected_output = "Top 3 cards: Favor, Exploding Camel, Skip" in
  assert_equal expected_output (see_the_future deck)

let see_the_future_test3 () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Alter the Future") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3 ] in
  let expected_output =
    "Top 3 cards: Water, Alter the Future, Exploding Camel"
  in
  assert_equal expected_output (see_the_future deck)

let see_the_future_test4 () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let card4 = make (Resource "Draw from Bottom") 1 in
  let deck = [ card1; card2; card3; card4 ] in
  let expected_output = "Top 3 cards: Water, Shuffle, Exploding Camel" in
  assert_equal expected_output (see_the_future deck)

let see_the_future_test4 () =
  let card1 = make (Resource "Shuffle") 1 in
  let card2 = make ExplodingCamel 1 in
  let deck = [ card1; card2 ] in
  let expected_output = "Top 3 cards: Shuffle, Exploding Camel" in
  assert_equal expected_output (see_the_future deck)

let see_the_future_test5 () =
  let card = make ExplodingCamel 1 in
  let deck = [ card ] in
  let expected_output = "Top 3 cards: Exploding Camel" in
  assert_equal expected_output (see_the_future deck)

let see_the_future_tests =
  [
    ("See The Future Test" >:: fun _ -> see_the_future_test ());
    ("See The Future Test 2" >:: fun _ -> see_the_future_test2 ());
    ("See The Future Test 3" >:: fun _ -> see_the_future_test3 ());
    ("See The Future Test 4" >:: fun _ -> see_the_future_test4 ());
  ]

(*************************** draw_from_bottom tests ***************************)

let draw_from_bottom_test1 () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Alter the Future") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3 ] in
  let expected_deck = [ card3; card1; card2 ] in
  "draw_from_bottom test" >:: fun _ ->
  assert_equal expected_deck (draw_from_bottom deck)

let draw_from_bottom_test2 () =
  let card = make ExplodingCamel 1 in
  let deck = [ card ] in
  let expected_deck = deck in
  "draw_from_bottom test" >:: fun _ ->
  assert_equal expected_deck (draw_from_bottom deck)

let draw_from_bottom_test =
  [ draw_from_bottom_test1 (); draw_from_bottom_test2 () ]

(*************************** string_of_card tests ***************************)
let test_string_of_card1 _ =
  let card = make ExplodingCamel 1 in
  assert_equal "Exploding Camel" (string_of_card card)

let test_string_of_card2 _ =
  let card = make Water 2 in
  assert_equal "Water" (string_of_card card)

let test_string_of_card3 _ =
  let card = make (Resource "See the Future") 1 in
  assert_equal "See the Future" (string_of_card card)

let test_string_of_card4 _ =
  let card = make (Resource "Alter the Future") 1 in
  assert_equal "Alter the Future" (string_of_card card)

let test_string_of_card5 _ =
  let card = make (Resource "Shuffle") 1 in
  assert_equal "Shuffle" (string_of_card card)

let test_string_of_card6 _ =
  let card = make (Resource "Bury") 1 in
  assert_equal "Bury" (string_of_card card)

let test_string_of_card7 _ =
  let card = make (Resource "Draw from Bottom") 1 in
  assert_equal "Draw from Bottom" (string_of_card card)

let string_of_card_test =
  [
    "String of Card 1" >:: test_string_of_card1;
    "String of Card 2" >:: test_string_of_card2;
    "String of Card 3" >:: test_string_of_card3;
    "String of Card 4" >:: test_string_of_card4;
    "String of Card 5" >:: test_string_of_card5;
    "String of Card 6" >:: test_string_of_card6;
    "String of Card 7" >:: test_string_of_card7;
  ]

(***************************bury******************************************)

let test_bury_empty_deck () =
  let result = bury_test [] 0 in
  assert_equal [] result

let test_bury_non_empty_deck () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3 ] in
  let new_deck = bury_test deck 3 in
  assert_equal [ card2; card3; card1; card3 ] new_deck

let test_bury_same_place () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3 ] in
  let new_deck = bury_test deck 1 in
  assert_equal deck new_deck

let test_bury_lots_of_cards () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3; card2; card2; card1; card1 ] in
  let new_deck = bury_test deck 3 in
  assert_equal
    [ card2; card3; card1; card3; card2; card2; card1; card1 ]
    new_deck

let test_bury_at_end () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let deck = [ card1; card2; card2; card2; card2; card2; card2; card2 ] in
  let new_deck = bury_test deck 8 in
  assert_equal
    [ card2; card2; card2; card2; card2; card2; card2; card1 ]
    new_deck

let test_bury_at_beginning () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3 ] in
  let new_deck = bury_test deck 1 in
  assert_equal deck new_deck

let test_bury_random_position () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3; card3 ] in
  let new_deck = bury_test deck 2 in
  assert_equal [ card2; card1; card3; card3 ] new_deck

let test_bury_single_card_deck () =
  let card = make Water 1 in
  let new_deck = bury_test [ card ] 0 in
  assert_equal [ card ] new_deck

let bury_tests =
  [
    ("Test_Bury NonEmpty" >:: fun _ -> test_bury_non_empty_deck ());
    ("Test_Bury Empty" >:: fun _ -> test_bury_empty_deck ());
    ("Test_Bury Same Place" >:: fun _ -> test_bury_same_place ());
    ("Test_Bury Lots of Cards" >:: fun _ -> test_bury_lots_of_cards ());
    ("Test_Bury At End" >:: fun _ -> test_bury_at_end ());
    ("Test_Bury At Beginning" >:: fun _ -> test_bury_at_beginning ());
    ("Test_Bury Random Position" >:: fun _ -> test_bury_random_position ());
    ("Test_Bury Single Card Deck" >:: fun _ -> test_bury_single_card_deck ());
  ]
(*************************** Player tests ******************************)

(*************************** Player tests ******************************)
let test_create () =
  let name = "Player1" in
  let hand = [] in
  let status = 0 in
  let player1 = create name hand status in
  assert_equal player1.name "Player1";
  assert_equal (get_hand_length player1) 7;
  (* Expecting 7 cards in the hand *)
  assert_equal player1.status 1

let test_create_multiple_cards () =
  let name = "Player2" in
  let hand = [] in
  let status = 1 in
  let player2 = create name hand status in
  assert_equal player2.name name;
  assert_equal true (has_card player2 (make Water 1));
  assert_equal player2.status status

let create_test =
  [
    ("create test" >:: fun _ -> test_create ());
    ("create test 2" >:: fun _ -> test_create_multiple_cards ());
  ]

(*************************** set_hand tests ******************************)

let test_set_hand () =
  let player = create "Player1" [] 0 in
  let new_hand = [ make (Resource "Shuffle") 1; make Water 1 ] in
  let updated_player = set_hand player new_hand in
  assert_equal player.name updated_player.name;
  assert_equal new_hand updated_player.hand;
  assert_equal player.status updated_player.status

let test_set_hand_empty_hand () =
  let player = create "Player1" [] 0 in
  let new_hand = [] in
  let updated_player = set_hand player new_hand in
  assert_equal player.name updated_player.name;
  assert_equal new_hand updated_player.hand;
  assert_equal player.status updated_player.status

let test_set_hand_multiple_cards () =
  let player = create "Player1" [] 0 in
  let new_hand =
    [ make (Resource "Shuffle") 1; make Water 1; make ExplodingCamel 1 ]
  in
  let updated_player = set_hand player new_hand in
  assert_equal player.name updated_player.name;
  assert_equal new_hand updated_player.hand;
  assert_equal player.status updated_player.status

let set_hand_tests =
  [
    ("Set Hand Test" >:: fun _ -> test_set_hand ());
    ("Set Hand Test - Empty Hand" >:: fun _ -> test_set_hand_empty_hand ());
    ( "Set Hand Test - Multiple Cards" >:: fun _ ->
      test_set_hand_multiple_cards () );
  ]

(*************************** add_card tests ******************************)

let test_add_card () =
  let player1 = create "Player1" [] 0 in
  let card = make Water 1 in
  let updated_player = add_card player1 card in
  assert_equal 8 (get_hand_length updated_player)

let test_add_card_empty_hand () =
  let player = create "Player1" [] 0 in
  let player_new = empty_hand player in
  let card = make (Resource "Shuffle") 1 in
  let updated_player = add_card player_new card in
  assert_equal player.name updated_player.name;
  assert_equal [ card ] updated_player.hand;
  assert_equal player.status updated_player.status

(*let test_add_card_multiple_cards () = let player = create "Player1" [ make
  Water 1; make (Resource "Shuffle") 0 ] 0 in let card1 = make ExplodingCamel 1
  in let card2 = make (Resource "Skip") 0 in let updated_player = add_card
  (add_card player card1) card2 in assert_equal player.name updated_player.name;
  assert_equal [ card1; card2; make Water 1; make (Resource "Shuffle") 0 ]
  updated_player.hand; assert_equal player.status updated_player.status

  let test_add_card_multiple_cards2 () = let player = create "Player1" [ make
  Water 1; make (Resource "Shuffle") 0 ] 0 in let card1 = make ExplodingCamel 1
  in let card2 = make (Resource "Skip") 0 in let updated_player = add_card
  (add_card player card1) card2 in assert_equal player.name updated_player.name;
  assert_equal [ card1; card2; make Water 1; make (Resource "Shuffle") 0 ]
  updated_player.hand; assert_equal player.status updated_player.status *)

let test_add_card_resource_card () =
  let player = create "Player1" [] 0 in
  let player_new = empty_hand player in
  let card = make (Resource "Alter the Future") 1 in
  let updated_player = add_card player_new card in
  assert_equal [ card ] updated_player.hand

let test_add_card_exploding_camel () =
  let player = create "Player1" [] 0 in
  let player_new = empty_hand player in
  let card = make ExplodingCamel 1 in
  let updated_player = add_card player_new card in
  assert_equal [ card ] updated_player.hand

let add_test =
  [
    ("Add Card Test" >:: fun _ -> test_add_card ());
    ("Add Card Test - Empty Hand" >:: fun _ -> test_add_card_empty_hand ());
    ("Add Card Test - Resource Card" >:: fun _ -> test_add_card_resource_card ());
    ( "Add Card Test - Exploding Camel" >:: fun _ ->
      test_add_card_exploding_camel () );
  ]

(*************************** remove_card tests ******************************)

let test_remove_card () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let player = create "Player1" [] 0 in
  let player_new = empty_hand player in
  let updated_1 = add_card player_new card1 in
  let updated_2 = add_card updated_1 card2 in
  let updated_player = remove_card updated_2 card1 in
  assert_equal [ card2 ] updated_player.hand

let test_remove_card_nonexistent_card () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 0 in
  let player = create "Player1" [] 0 in
  let player_new = empty_hand player in
  let updated_1 = add_card player_new card1 in
  let updated_2 = add_card updated_1 card2 in
  let card_to_remove = make (Resource "Skip") 1 in
  let updated_player = remove_card updated_2 card_to_remove in
  assert_equal ~printer:string_of_bool false
    (List.mem card_to_remove updated_player.hand);
  assert_equal ~printer:string_of_bool true (List.mem card1 updated_player.hand);
  assert_equal ~printer:string_of_bool true (List.mem card2 updated_player.hand)

let test_remove_card_last_card () =
  let card = make Water 1 in
  let player = create "Player1" [ card ] 0 in
  let updated_player = remove_card player card in
  assert_equal ~printer:string_of_bool false (List.mem card updated_player.hand)

let test_remove_card_last_card2 () =
  let card = make Water 1 in
  let player = create "Player1" [ card ] 0 in
  let updated_player = remove_card player card in
  assert_equal ~printer:string_of_bool false (has_card updated_player card)

let test_remove_card_single_card () =
  let card = make Water 1 in
  let player = create "Player1" [ card ] 0 in
  let updated_player = remove_card player card in
  assert_equal ~printer:string_of_bool false (has_card updated_player card)

let remove_card_test =
  [
    ("Remove Card Test" >:: fun _ -> test_remove_card ());
    ( "Remove Card Test - Nonexistent Card" >:: fun _ ->
      test_remove_card_nonexistent_card () );
    ("Remove Card Test - Last Card" >:: fun _ -> test_remove_card_last_card ());
    ( "Remove Card Test - Last Card 2" >:: fun _ ->
      test_remove_card_last_card2 () );
    ( "Remove Card Test - Single Card" >:: fun _ ->
      test_remove_card_single_card () );
  ]

(*************************** has_card tests ******************************)

let test_has_card () =
  let card = make Water 1 in
  let player = create "Player1" [] 0 in
  assert (has_card player card)

let test_has_card_empty_hand () =
  let card = make Water 1 in
  let player = create "Player1" [] 0 in
  let empty_player = empty_hand player in
  assert_equal false (has_card empty_player card)

let test_has_card_multiple_cards () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 0 in
  let player = create "Player1" [] 0 in
  let empty_player = empty_hand player in
  let player = add_card empty_player card1 in
  let player = add_card player card2 in
  assert_equal true (has_card player card1);
  assert_equal true (has_card player card2)

let has_card_test =
  [
    ("Has Card Test" >:: fun _ -> test_has_card ());
    ("Has Card Test - Empty Hand" >:: fun _ -> test_has_card_empty_hand ());
    ( "Has Card Test - Multiple Cards" >:: fun _ ->
      test_has_card_multiple_cards () );
  ]

(*************************** get_hand tests ******************************)
let check_hands lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

let test_get_hand_empty_hand () =
  let player = empty_hand (Player.create "Player1" [] 0) in
  assert_equal [] (get_hand player)

let test_get_hand_multiple_cards () =
  let hand = [ make Water 1; make (Resource "Shuffle") 0 ] in
  let player = Player.create "Player1" hand 0 in
  let player = set_hand player hand in
  assert (check_hands (get_hand player) hand)

let test_get_hand () =
  let hand = [ make Water 1; make (Resource "Shuffle") 1 ] in
  let player = Player.create "Player1" hand 0 in
  let player = set_hand player hand in
  assert (check_hands (get_hand player) hand)

let get_hand_test =
  [
    ("Get Hand Test" >:: fun _ -> test_get_hand ());
    ("Get Hand Test - Empty Hand" >:: fun _ -> test_get_hand_empty_hand ());
    ( "Get Hand Test - Multiple Cards" >:: fun _ ->
      test_get_hand_multiple_cards () );
  ]

(*************************** get_name tests ******************************)

let test_get_name () =
  let player = create "Player1" [] 0 in
  assert_equal (get_name player) "Player1"

let test_get_name_non_empty_name () =
  let player = create "Player2" [] 0 in
  assert_equal "Player2" (get_name player)

let test_get_name_empty_name () =
  let player = create "" [] 0 in
  assert_equal "" (get_name player)

let get_name_test =
  [
    ("Get Name Test1" >:: fun _ -> test_get_name ());
    ( "Get Name Test2 - Non-Empty Name" >:: fun _ ->
      test_get_name_non_empty_name () );
    ("Get Name Test - Empty Name" >:: fun _ -> test_get_name_empty_name ());
  ]

(************** count_active_players tests ******************************)

let test_count_active_players_empty_list () =
  let players = [] in
  let result = count_active_players players in
  assert_equal 0 result

let test_count_active_players_all_active_players () =
  let players =
    [ create "Player1" [] 1; create "Player2" [] 1; create "Player3" [] 1 ]
  in
  let result = count_active_players players in
  assert_equal 3 result

let test_count_active_players_some_active_players () =
  let player1 = create "Player1" [] 1 in
  let player2 = create "Player2" [] 1 in
  let player3 = create "Player3" [] 1 in
  let player4 = create "Player4" [] 1 in
  let players =
    [
      { player1 with status = 0 }; { player2 with status = 0 }; player3; player4;
    ]
  in
  let result = count_active_players players in
  assert_equal 2 result

let count_active_players_tests =
  [
    ( "Count Active Players Test - Empty List" >:: fun _ ->
      test_count_active_players_empty_list () );
    ( "Count Active Players Test - All Active Players" >:: fun _ ->
      test_count_active_players_all_active_players () );
    ( "Count Active Players Test - Some Active Players" >:: fun _ ->
      test_count_active_players_some_active_players () );
  ]

(************************ GAMEPLAY FUNCTION TESTS ***************************)

let test_create_players1 _ =
  let players = create_players_test 1 in
  assert_equal 1 (List.length players)

let test_create_players3 _ =
  let players = create_players_test 3 in
  assert_equal 3 (List.length players)

let test_create_players10 _ =
  let players = create_players_test 10 in
  assert_equal 10 (List.length players)

let create_players_test =
  [
    ("Create Player 1" >:: fun _ -> test_create_players1 ());
    ("Create Player 1" >:: fun _ -> test_create_players3 ());
    ("Create Player 1" >:: fun _ -> test_create_players10 ());
  ]

let test_pop_deck_1 _ =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card3; card2; card1 ] in
  let popped_deck = pop_deck_test deck in
  assert_equal [ card2; card1 ] popped_deck

let test_pop_deck_2 _ =
  let card1 = make Water 1 in
  let card2 = make (Resource "Shuffle") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3 ] in
  let popped_deck = pop_deck_test deck in
  assert_equal [ card2; card3 ] popped_deck

let test_pop_deck_empty_deck () =
  let popped_deck = pop_deck_test [] in
  assert_equal [] popped_deck

let test_pop_deck_single_card_deck () =
  let card = make Water 1 in
  let popped_deck = pop_deck_test [ card ] in
  assert_equal [] popped_deck

let pop_deck_test =
  [
    ("test pop deck 1" >:: fun _ -> test_pop_deck_1 ());
    ("test pop deck 2" >:: fun _ -> test_pop_deck_2 ());
    ("Pop Deck Test - Empty Deck" >:: fun _ -> test_pop_deck_empty_deck ());
    ( "Pop Deck Test - Single Card Deck" >:: fun _ ->
      test_pop_deck_single_card_deck () );
  ]

(*****************remove at index test**************)

let remove_at_test_empty_list _ = assert_equal [] (remove_at 0 [])

let remove_at_test_remove_first_element () =
  let input_list = [ 1; 2; 3; 4; 5 ] in
  let expected_output = [ 2; 3; 4; 5 ] in
  let actual_output = remove_at 0 input_list in
  assert_equal
    ~cmp:(fun lst1 lst2 -> List.sort compare lst1 = List.sort compare lst2)
    expected_output actual_output

let remove_at_test_remove_last_element () =
  let input_list = [ 1; 2; 3; 4; 5 ] in
  let expected_output = [ 1; 2; 3; 4 ] in
  let actual_output = remove_at 4 input_list in
  assert_equal
    ~cmp:(fun lst1 lst2 -> List.sort compare lst1 = List.sort compare lst2)
    expected_output actual_output

let remove_at_test_index_out_of_range _ =
  let input_list = [ 1; 2; 3; 4; 5 ] in
  assert_equal input_list (remove_at 10 input_list)

let remove_at_test_negative_index () =
  let input_list = [ 1; 2; 3; 4; 5 ] in
  assert_equal input_list (remove_at (-1) input_list)

let remove_at_test_middle_index () =
  let input_list = [ 1; 2; 3; 4; 5 ] in
  let expected_output = [ 1; 2; 4; 5 ] in
  let actual_output = remove_at 2 input_list in
  assert_equal
    ~cmp:(fun lst1 lst2 -> List.sort compare lst1 = List.sort compare lst2)
    expected_output actual_output

let remove_at_tests =
  [
    ( "Remove At Test - Negative Index" >:: fun _ ->
      remove_at_test_negative_index () );
    ("Remove At Test - Middle Index" >:: fun _ -> remove_at_test_middle_index ());
    ("Remove At Test - Empty List" >:: fun _ -> remove_at_test_empty_list ());
    ( "Remove At Test - Remove First Element" >:: fun _ ->
      remove_at_test_remove_first_element () );
    ( "Remove At Test - Remove Last Element" >:: fun _ ->
      remove_at_test_remove_last_element () );
    ( "Remove At Test - Index Out of Range" >:: fun _ ->
      remove_at_test_index_out_of_range () );
  ]

(*************************** draw_card tests ******************************)

let test_draw_card_no_card _ =
  let player = create "Player1" [] 0 in
  let result = draw_card_test player [] in
  assert_equal NoCard result

let test_draw_card_exploding_camel _ =
  let player = create "Player1" [] 0 in
  let card = make ExplodingCamel 1 in
  let result = draw_card_test player [ card ] in
  assert_equal ExplodingCamel result

let test_draw_card_another_card _ =
  let player = create "Player1" [] 0 in
  let card = make Water 1 in
  let result = draw_card_test player [ card ] in
  match result with
  | AnotherCard updated_player ->
      assert_equal "Player1" (get_name updated_player)
  | _ -> assert_failure "Expected AnotherCard but got a different result"

let draw_card_tests =
  [
    "Draw Card - No Card" >:: test_draw_card_no_card;
    "Draw Card - Exploding Camel" >:: test_draw_card_exploding_camel;
    "Draw Card - Another Card" >:: test_draw_card_another_card;
  ]

let test_play_draw _ =
  let card = make (Resource "Draw") 1 in
  let card1 = make Water 3 in
  let card2 = make Water 2 in
  let deck = [ card1; card2; card1 ] in
  let player = { name = "Player1"; hand = [ card ]; status = 0 } in
  let index = 1 in
  let updated_players, updated_deck, _ = play_draw card deck player index in
  assert (deck <> updated_deck)

let test_play_draw2 _ =
  let card = make (Resource "Draw") 1 in
  let card1 = make Water 3 in
  let card2 = make Water 2 in
  let deck = [ card1; card2; card1 ] in
  let player = { name = "Player1"; hand = [ card ]; status = 0 } in
  let index = 1 in
  let updated_players, updated_deck, _ = play_draw card deck player index in
  assert (deck <> updated_deck)

let play_draw_tests =
  [ "Play Draw" >:: test_play_draw; "Play Draw" >:: test_play_draw2 ]

(***************game_status*******************)

let player1 = create "Player1" [] 0
let player2 = create "Player2" [] 0
let player3 = create "Player3" [] 0
let player4 = create "Player4" [] 0
let deck = init_deck 4
let game_status = start_game 4 [ player1; player2; player3; player4 ] deck

(************************* Get Statements Tests ****************************)

(* get_num_player *)
let test_get_num_player () =
  let expected_num_players = 4 in
  assert_equal expected_num_players (get_num_player game_status)

(* get_players *)
let test_get_players () =
  let expected_players = [ player1; player2; player3; player4 ] in
  assert_equal expected_players (get_players game_status)

(* get_deck *)
let test_get_deck () =
  let expected_deck = deck in
  assert_equal expected_deck (get_deck game_status)

(* is_game_start *)
let test_is_game_start () =
  let expected_game_start = true in
  assert_equal expected_game_start (is_game_start game_status)

(* get_winner *)
let test_get_winner () =
  let expected_winner = None in
  assert_equal expected_winner (get_winner game_status)

(* get_current_player *)
let test_get_current_player () =
  let expected_current_player = player1 in
  assert_equal expected_current_player (get_current_player game_status)

(* is_skip_next_turn *)
let test_is_skip_next_turn () =
  let expected_skip_next_turn = false in
  assert_equal expected_skip_next_turn (is_skip_next_turn game_status)

let game_status_tests =
  [
    ("get_num_player" >:: fun _ -> test_get_num_player ());
    ("get_players" >:: fun _ -> test_get_players ());
    ("get_deck" >:: fun _ -> test_get_deck ());
    ("is_game_start" >:: fun _ -> test_is_game_start ());
    ("get_winner" >:: fun _ -> test_get_winner ());
    ("get_current_player" >:: fun _ -> test_get_current_player ());
    ("is_skip_next_turn" >:: fun _ -> test_is_skip_next_turn ());
  ]

(*********************GAME STATUS TEST 2*********************)
let card1 = make (Resource "Card1") 1
let card2 = make Water 3
let card3 = make Water 2
let card4 = make (Resource "Card2") 2
let card5 = make Water 4
let card6 = make (Resource "Card3") 3
let card7 = make (Resource "Card4") 4
let card8 = make Water 5
let player1 = create "Player1" [ card1; card2 ] 0
let player2 = create "Player2" [ card3; card4 ] 0
let player3 = create "Player3" [ card5; card6 ] 0
let player4 = create "Player4" [ card7; card8 ] 0
let deck = init_deck 4
let game_status = start_game 4 [ player1; player2; player3; player4 ] deck

(* Add the remaining test cases as before *)

(* get_num_player *)
let test_get_num_player () = assert_equal 4 (get_num_player game_status)

(* get_players *)
let test_get_players () =
  let expected_players = [ player1; player2; player3; player4 ] in
  assert_equal expected_players (get_players game_status)

(* get_deck *)
let test_get_deck () = assert_equal deck (get_deck game_status)

(* is_game_start *)
let test_is_game_start () = assert_equal true (is_game_start game_status)

(* get_winner *)
let test_get_winner () = assert_equal None (get_winner game_status)

(* get_current_player *)
let test_get_current_player () =
  assert_equal player1 (get_current_player game_status)

(* is_skip_next_turn *)
let test_is_skip_next_turn () =
  assert_equal false (is_skip_next_turn game_status)

let game_status_tests2 =
  [
    ("get_num_player" >:: fun _ -> test_get_num_player ());
    ("get_players" >:: fun _ -> test_get_players ());
    ("get_deck" >:: fun _ -> test_get_deck ());
    ("is_game_start" >:: fun _ -> test_is_game_start ());
    ("get_winner" >:: fun _ -> test_get_winner ());
    ("get_current_player" >:: fun _ -> test_get_current_player ());
    ("is_skip_next_turn" >:: fun _ -> test_is_skip_next_turn ());
  ]

(**************************GAMEPLAY STATUS TEST 3
  ************************************)
let make_test_card resource value = make (Resource resource) value
let create_test_player name cards = create name cards 0

(* Test cards *)
let card1 = make_test_card "Card1" 1
let card2 = make Water 3
let card3 = make Water 2
let card4 = make_test_card "Card2" 2
let card5 = make Water 4
let card6 = make_test_card "Card3" 3
let card7 = make_test_card "Card4" 4
let card8 = make Water 5

(* Test players *)
let player1 = create_test_player "Player1" [ card1; card2 ]
let player2 = create_test_player "Player2" [ card3; card4 ]
let player3 = create_test_player "Player3" [ card5; card6 ]
let player4 = create_test_player "Player4" [ card7; card8 ]
let player5 = create_test_player "Player5" []
let player6 = create_test_player "Player6" [ card1; card2; card3 ]
let player7 = create_test_player "Player7" []
let player8 = create_test_player "Player8" [ card4; card5; card6 ]
let player9 = create_test_player "Player9" [ card7; card8 ]

let player10 =
  create_test_player "Player10" [ card1; card2; card3; card4; card5 ]

(* Test deck *)
let deck = init_deck 10

(* Game status with test players and deck *)
let game_status =
  start_game 10
    [
      player1;
      player2;
      player3;
      player4;
      player5;
      player6;
      player7;
      player8;
      player9;
      player10;
    ]
    deck

let game_status_tests3 =
  [
    ("get_num_player" >:: fun _ -> assert_equal 10 (get_num_player game_status));
    ( "get_players" >:: fun _ ->
      let expected_players =
        [
          player1;
          player2;
          player3;
          player4;
          player5;
          player6;
          player7;
          player8;
          player9;
          player10;
        ]
      in
      assert_equal expected_players (get_players game_status) );
    ("get_deck" >:: fun _ -> assert_equal deck (get_deck game_status));
    ("is_game_start" >:: fun _ -> assert_equal true (is_game_start game_status));
    ("get_winner" >:: fun _ -> assert_equal None (get_winner game_status));
    ( "get_current_player" >:: fun _ ->
      assert_equal player1 (get_current_player game_status) );
    ( "is_skip_next_turn" >:: fun _ ->
      assert_equal false (is_skip_next_turn game_status) );
  ]

(*************************** deck_length tests ***************************)

let deck_length_test1 () =
  let card1 = make Water 1 in
  let card2 = make (Resource "Alter the Future") 1 in
  let card3 = make ExplodingCamel 1 in
  let deck = [ card1; card2; card3 ] in
  let expected_length = 3 in
  "deck_length test" >:: fun _ ->
  assert_equal expected_length (deck_length deck)

let deck_length_test2 () =
  let deck = [] in
  let expected_length = 0 in
  "deck_length test" >:: fun _ ->
  assert_equal expected_length (deck_length deck)

let deck_length_test3 () =
  let card = make ExplodingCamel 1 in
  let deck = [ card ] in
  let expected_length = 1 in
  "deck_length test" >:: fun _ ->
  assert_equal expected_length (deck_length deck)

let deck_length_tests =
  [ deck_length_test1 (); deck_length_test2 (); deck_length_test3 () ]

let suite =
  "test suite"
  >::: List.flatten
         [
           make_tests;
           shuffle_deck_tests;
           init_deck_tests;
           see_the_future_tests;
           draw_from_bottom_test;
           string_of_card_test;
           create_test;
           add_test;
           remove_card_test;
           has_card_test;
           get_hand_test;
           get_name_test;
           create_players_test;
           pop_deck_test;
           draw_card_tests;
           remove_at_tests;
           game_status_tests;
           bury_tests;
           game_status_tests2;
           play_draw_tests;
           game_status_tests3;
           deck_length_tests;
           set_hand_tests;
           count_active_players_tests;
           get_hand_length_tests;
           set_hand_tests;
         ]

let () = run_test_tt_main suite
