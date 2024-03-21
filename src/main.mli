(* main.mli *)

open Lib.Cards
open Lib.Player

(** [section_title s] creates a section title widget. *)
val section_title : string -> Bogue.Layout.t

(** [hline width] creates a horizontal line widget with the given width. *)
val hline : int -> Bogue.Layout.t

(** [empty_window num_players] creates an empty game window with tabs for players. *)
val empty_window : int -> unit

(*
(** [create_players num_players] creates a list of players. *)
val create_players : int -> Lib.Player.t list

(** [pop_deck deck] removes the top card from the deck. *)
val pop_deck : card list -> 'a list

(** [draw_card player deck] simulates a player drawing a card. *)
val draw_card : Lib.Player.t -> card list -> Lib.Player.t list * card list

(** [remove_at index lst] removes the element at the given index from the list. *)
val remove_at : int -> 'a list -> 'a list

(** [play_card player index deck] simulates a player playing a card. *)
val play_card : Lib.Player.t -> int -> card list -> Lib.Player.t list * card list

(** [player_interface player rest_players deck] creates a player interface. *)
val player_interface : Lib.Player.t -> Lib.Player.t list -> card list -> unit

(** [game_loop players deck] is the main game loop. *)
val game_loop : Lib.Player.t list -> card list -> unit

(** Main function to start the game. *)
val main : unit -> unit*)