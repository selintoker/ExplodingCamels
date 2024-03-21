open Cards
open Player

val read_valid_int : unit -> int
(** [read_valid_int ()] reads an integer from the user and ensures that the
    input is a valid integer. If an invalid input (non-integer) is provided, the
    user is prompted to enter a valid number.

    @raise Failure
      "int_of_string" if the entered value cannot be converted to an integer. *)

val create_players_test : int -> player list
(** [create_players_test num_players] generates a list of test players. *)

(** [draw_result_test] represents possible outcomes of drawing a card during
    testing. *)
type draw_result_test =
  | NoCard
  | ExplodingCamel
  | AnotherCard of player

val pop_deck_test : card list -> card list
(** [pop_deck_test deck] simulates popping a card from the deck during testing. *)

val draw_card_test : player -> card list -> draw_result_test
(** [draw_card_test player deck] simulates drawing a card for a player during
    testing. *)

val create_players : int -> player list
(** [create_players num_players] initializes a list of players for the game. *)

val pop_deck : card list -> card list
(** [pop_deck deck] removes the top card from the deck. *)

val remove_at : int -> 'a list -> 'a list
(** [remove_at index lst] removes an element at a specific index. *)

val update_hand_after_play : player -> int -> card list
(** [update_hand_after_play] returns the player's hand after playing a card at
    the specified index given the variable type [player], index [index] *)

val get_valid_target_player : player list -> player
(** [get_valid_target_player players] selects a valid target player for a game
    action. *)

val play_see_the_future :
  card list -> player -> int -> player list * card list * bool
(** [play_see_the_future deck player num_players] simulates playing the "See the
    Future" card. *)

val play_alter_the_future :
  card list -> player -> int -> player list * card list * bool
(** [play_alter_the_future deck player num_players] simulates playing the "Alter
    the Future" card. *)

val play_shuffle : card list -> player -> int -> player list * card list * bool
(** [play_shuffle deck player num_players] simulates playing the "Shuffle" card. *)

val play_draw_from_bottom :
  card list -> player -> int -> player list * card list * bool
(** [play_draw_from_bottom deck player num_players] simulates playing the "Draw
    From Bottom" card. *)

val play_skip : card list -> player -> int -> player list * card list * bool
(** [play_skip deck player num_players] simulates playing the "Skip" card. *)

val play_bury : card list -> player -> int -> player list * card list * bool
(** [play_bury deck player num_players] simulates playing the "Bury" card. *)

val play_steal :
  player list -> card list -> player -> int -> player list * card list * bool
(** [play_steal players deck player num_players] simulates playing the "Steal"
    card. *)

val play_draw :
  card -> card list -> player -> int -> player list * card list * bool
(** [play_draw card deck player num_players] simulates playing a regular draw
    card. *)
