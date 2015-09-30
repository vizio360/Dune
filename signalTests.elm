import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Random exposing (..)


main : Signal Element
main =
  -- map3 gameLoop (fps 30) (every second) Mouse.position
  map draw clicked

{--}
clicked : Signal (Time, (Int, Int))
clicked = 
  sampleOn Mouse.clicks (timestamp Mouse.position)
  
collageWidth = 700
collageHeight = 700
draw (t, p) =
  collage collageWidth collageHeight (showMatrix t p) 

showMatrix d c =
  let
    seed = initialSeed (truncate d)
    (red, seed1) = generate (int 0 255) seed
    (green, seed2) = generate (int 0 255) seed1
    (blue, _) = generate (int 0 255) seed2
  in
    [rect 70 70
      |> filled (rgba red green blue 0.9)
      |> move ((toFloat (fst c))-collageWidth/2,-(toFloat (snd c))+collageHeight/2)]
--}
