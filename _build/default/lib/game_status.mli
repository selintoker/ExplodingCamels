open Cards
open Player

type game_status
(** A [Game Status] represents a game with includes the number of players, a
    list of player information, the deck of cards, whether the game has started,
    the winner -if the game is over-, the current player, and if the next turn
    should be skipped *)

val start_game : int -> player list -> card list -> game_status
(** [start_game num_players players deck] initializes a new game. *)

val end_game : game_status -> string
(** [end_game game] determines the end result of the game. *)

val check_hand : player list -> string
(** [check_hand players] inspects the hands of the players. *)

val get_num_player : game_status -> int
(** [get_num_player game] returns the count of players in the game. *)

val get_players : game_status -> player list
(** [get_players game] retrieves the list of players in the game. *)

val get_deck : game_status -> card list
(** [get_deck game] fetches the deck of cards in the game. *)

val is_game_start : game_status -> bool
(** [is_game_start game] checks if the game has started. *)

val get_winner : game_status -> player option
(** [get_winner game] returns the winner of the game, if it has concluded. *)

val get_current_player : game_status -> player
(** [get_current_player game] returns the current player taking their turn in
    the game. *)

val is_skip_next_turn : game_status -> bool
(** [is_skip_next_turn game] checks if the next turn should be skipped. *)
