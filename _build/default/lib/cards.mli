type card
(** A [Card] represents a playing card with a unique identifier, type, and
    functionality. *)

(** The possible types of cards in the game. *)
type card_type =
  | ExplodingCamel
  | Water
  | Resource of string

val make : card_type -> int -> card
(** [make card_type card_id] creates a new card with the specified type and
    unique identifier. *)

val name : card -> int
(** [name card] returns the unique identifier of the card. *)

val functionality : card -> string
(** [functionality card] returns the functionality description of the card. *)

val deck : card -> bool
(** [deck card] returns true if the card is in the deck, false otherwise. *)

val return_card_type : card -> card_type
(** [return_card_type card] returns the type of the card. *)

val init_deck : int -> card list
(** [init_deck num_players] initializes a deck of cards for [num_players]. *)

val shuffle_deck : card list -> card list
(** [shuffle_deck deck] shuffles the order of cards in the deck. *)

val split_at : int -> 'a list -> 'a list * 'a list
(** [split_at index lst] splits a list at a specific index. *)

val string_of_card : card -> string
(** [string_of_card card] converts a card to a string representation. *)

val print_deck : card list -> unit
(** [print_deck deck] prints the entire deck of cards. *)

val see_the_future : card list -> string
(** [see_the_future deck] views the top three cards in the deck. *)

val draw_from_bottom : card list -> card list
(** [draw_from_bottom deck] draws a card from the bottom of the deck. *)

val alter_the_future : card list -> card list
(** [alter_the_future deck] alters the order of the top three cards in the deck
    based on user input. *)

val bury : card list -> card list
(** [bury deck] draws the top card from the deck and buries it back at any
    position. *)

val bury_test : card list -> int -> card list
(** [bury_test deck where] simulates the burying of the top card in the deck at
    a specified position. *)

val deck_length : 'a list -> int
(** [deck_length deck] is the number of cards in the deck. *)

val print_deck_length : card list -> unit
(** Print the length of the deck *)
