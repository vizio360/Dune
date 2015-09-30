module Board where

type alias Characteristics = {name:String, attack:Int, energy:Int} 

type Cell = Monster Characteristics | Door | Cleared | Player Characteristics



