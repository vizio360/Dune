import Graphics.Element exposing (Element, show)
import Color exposing (..)
import Matrix as M
import Graphics.Collage exposing (..)
import Html exposing (..)
import Random exposing (..)
import Signal exposing (..)

main : Signal Element
main =
  map gameLoop (fps 30)
  collage 700 700 (showMatrix m 0 0 70)


  
type alias RandomModel = {value:Int, seed:Seed}
randomModel: RandomModel
randomModel = {value=0, seed=initialSeed(345435)}
getRandomValue: Int -> Int -> RandomModel -> Int
getRandomValue min max model =
  let
    (value, seed) = generate (int min max) model.seed
  in
    {model| seed <- seed}
    value
    
createColor =
  let
    {red, seed1} = getRandomValue 0 250 randomModel
    {green, seed2} = getRandomValue 0 250 {randomModel| seed <- seed1}
    {blue, seed3} = getRandomValue 0 250 {randomModel| seed <- seed2}
  in
    {randomModel| seed <- seed3}
    rgba red green blue 0.6


showMatrix: M.Matrix String -> Float -> Float -> Float -> List Form
showMatrix m row col w =
  let
     el = M.get (M.loc (truncate row) (truncate col)) m
     width = M.colCount m
     height = M.rowCount m
     createShape e = [ rect w w
                         |> filled createColor()
                         |> move (col*w, row*w) 
                       ,
                       toForm( show e )
                         |> move (col*w, row*w)
                     ]
  in
     case el of
       Just a -> createShape(a)++(showMatrix m row (col+1) w)
       Nothing -> if | row == (toFloat height) -> []
                     | otherwise -> (showMatrix m (row+1) 0 w)

m = M.matrix 3 5 (\x -> toString x)

