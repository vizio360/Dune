module Levels.Level1 where
import Matrix exposing (..)
import Board exposing (..)


initLevel : Board.Cell -> (Matrix Board.Cell, (Int, Int))


initLevel player =
  let
    populate loc =
      case loc of
        (0, 0) -> Board.Door
        (2, 4) -> player
        (_,_) -> Board.Monster (Board.Characteristics ("M "++(toString loc)) 5 5) 
  in
    (matrix 4 5 populate, (2, 4))

getPlayer: Matrix Board.Cell -> Board.Cell
getPlayer level =
  let
    isPlayer el =
      case el of
        Board.Player p -> True
        _ -> False
    results = List.filter isPlayer (flatten level)
  in
    case List.head results of
      Nothing -> Board.Player {name="DEFAULT", attack=0, energy=0}
      Just a -> a

  

movePlayerTo currentPlayer currentPlayerLocation row col level =
  let
    player = get currentPlayerLocation level
  in
    case player of
      Nothing -> level
      Just a ->
        let
          tmp = set (loc row col) currentPlayer level
        in
          set currentPlayerLocation Board.Cleared tmp
 
