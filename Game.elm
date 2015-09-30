import Board exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Levels.Level1 as CurrentLevel
import Color exposing (..)
import Matrix as M
import Signal exposing (..)
import Mouse
import Debug
import Time
import Markdown

type GameState = PLAY | GAMEOVER | LEVEL_COMPLETED
player = Player (Characteristics "Bob" 10 50)
(level, playerLocation) = CurrentLevel.initLevel player
model = { level=level, playerLocation=playerLocation, moveTimeout=0, gameState=PLAY}
-- try to add a moveTimeout in the model
-- add a new mapping to a Time signal (maybe with a merge)

timeoutMoveConstant = 10
main: Signal Element
main =
  map view (foldp update model input) 

view model =
  case model.gameState of
    PLAY ->
      updateCanvas model
    GAMEOVER ->
      gameOver()
    LEVEL_COMPLETED ->
      levelCompleted ()

gameOver ()  =
  collage 1000 1000 [toForm (show "GAME OVER!")]

levelCompleted () =
  collage 1000 1000 [toForm (show "Level completed!")]

update someInput model =
  case someInput of
    Click (x,y) ->
      processClick (x,y) model
    Timeout t ->
      let
        timeout = model.moveTimeout+1
      in
        {model| moveTimeout <- model.moveTimeout+1
              , gameState <- if timeout > timeoutMoveConstant then GAMEOVER else model.gameState}
      

processClick mousePosition model =
  let
    level = model.level
    width = (M.colCount level) - 1
    height = (M.rowCount level) - 1
    (x, y) = mousePosition
    col = if (x > 449 && x < 951) then (x - 450)//100 else -1
    row = if (y > 49 && y < 551) then (550 - y)//100  else -1
    el = M.get (M.loc row col) level
  in
    case el of
      Nothing -> model
      Just a ->
        case a of
          Board.Cleared -> model
          cell ->
            if | (isNextToPlayer model.playerLocation row col) ->
                case cell of
                  Board.Door -> {model| gameState <- LEVEL_COMPLETED}
                  Board.Monster monster ->
                    let
                      attack a b =
                        if a.energy <=0 || b.energy <=0 then
                           a
                        else
                          attack {a| energy <- a.energy - b.attack} {b| energy <- b.energy - a.attack}
                          
                      player = case CurrentLevel.getPlayer level of
                                 Player p -> p
                      playerResult = attack player monster
                    in
                      if (playerResult.energy <= 0) then
                        {model| gameState <- GAMEOVER}
                      else
                        {model| level <- CurrentLevel.movePlayerTo (Board.Player playerResult) model.playerLocation row col level
                              , playerLocation <- (row, col)
                              , moveTimeout <- 0} -- resetting move timeout after valid move
               | otherwise -> model

    
isNextToPlayer playerLocation row col =
  let
    (playerX, playerY) = playerLocation
    rowDiff = (abs (row-playerX))
    colDiff = (abs (col-playerY))
  in
    case (rowDiff, colDiff)of
        (0, 1) -> True
        (1, 0) -> True
        (_,_) -> False
        
type Updates = Click (Int, Int) | Timeout Int
input = 
  merge
    (map (\p -> Click p) (sampleOn Mouse.clicks Mouse.position))
    (map (\t -> Timeout 1) (Time.every Time.second))

updateCanvas model =
  collage 1000 1000 ((renderTimeout model.moveTimeout)++(renderLevel model.level))

renderTimeout t =
  [toForm (show ("Move timeout: "++(toString t)))
    |> move (-400, 400)]

renderLevel level =
  let
    cellWidth = 100
    showMatrix m row col =
      let
        el = M.get (M.loc (truncate row) (truncate col)) m
        width = M.colCount m
        height = M.rowCount m
        createShape e color = [ square cellWidth
                                 |> filled color
                                 |> move (col*cellWidth, row*cellWidth) 
                               ,
                                square cellWidth
                                 |> outlined (solid (rgba 255 255 255 1))
                                 |> move (col*cellWidth, row*cellWidth) 
                               ,

                                  toForm( Markdown.toElement e )
                                  |> move (col*cellWidth, row*cellWidth)
                              ]
        showCharact x =
          ("A: "++(toString x.attack)++"<br/>E: "++(toString x.energy))
      in
        case el of
          Just a -> 
            case a of
              Door ->
                (createShape "D" (rgba 100 100 255 0.8))++(showMatrix m row (col+1))
              Cleared ->
                (createShape "C" (rgba 100 100 100 0.8))++(showMatrix m row (col+1))
              Player p ->
                (createShape (showCharact p) (rgba 100 255 100 0.8))++(showMatrix m row (col+1))
              Monster monster ->
                (createShape (showCharact monster) (rgba 255 100 100 0.8))++(showMatrix m row (col+1))
          Nothing -> if | row == (toFloat height) -> []
                        | otherwise -> (showMatrix m (row+1) 0)
  in
    showMatrix level 0 0
