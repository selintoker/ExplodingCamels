open Cards

type player = {
  name : string;
  hand : card list;
  status : int;
}
(** [type player] represents a player in the game with a [name], a [hand] of
    cards, and a [status]. *)

val create : string -> card list -> int -> player
(** [create name hand status] creates a player with the given [name], initial
    [hand], and [status]. *)

val add_card : player -> card -> player
(** [add_card player card] adds a [card] to the player's hand. *)

val remove_card : player -> card -> player
(** [remove_card player card] removes a specific [card] from the player's hand,
    if it exists. *)

val has_card : player -> card -> bool
(** [has_card player card] checks if the player's hand contains a specific
    [card]. *)

val get_hand : player -> card list
(** [get_hand player] retrieves the player's hand as a list of cards. *)

val get_hand_length : player -> int
(** [get_hand_length player] retrieves the number of cards in player's hand *)

val get_name : player -> string
(** [get_name player] retrieves the player's name. *)

val empty_hand : player -> player
(** [empty_hand player] creates a new player with the same name and status as
    [player], but with an empty hand. *)

val set_hand : player -> card list -> player
(** [set_hand player new_hand] creates a new player with the same name and
    status as [player], but with the hand set to [new_hand]. *)

val count_active_players : player list -> int
(** [count_active_players players] counts the number of players in the list
    [players] with a status of 1. *)
