open Cards
open Player

type game_status = {
  num_player : int;
  players : player list;
  deck : card list;
  game_start : bool;
  winner : player option;
  current_player : player;
  skip_next_turn : bool;
}

let start_game (n : int) (players : player list) (deck : card list) =
  {
    num_player = n;
    players;
    deck;
    game_start = true;
    winner = None;
    current_player = List.hd players;
    skip_next_turn = false;
  }

let end_game (a : game_status) : string =
  (* when there is only one player left in the game*)
  get_name (List.hd a.players)

(* add more conditions to end the game*)
let rec check_hand (lst : player list) : string =
  match lst with
  | hd :: tl ->
      if List.length (Player.get_hand hd) = 0 then get_name hd
      else check_hand tl
  | [] -> ""

(* Get statements *)

let get_num_player (gs : game_status) : int = gs.num_player

let get_players (gs : game_status) : player list = gs.players

let get_deck (gs : game_status) : card list = gs.deck

let is_game_start (gs : game_status) : bool = gs.game_start

let get_winner (gs : game_status) : player option = gs.winner

let get_current_player (gs : game_status) : player = gs.current_player

let is_skip_next_turn (gs : game_status) : bool = gs.skip_next_turn