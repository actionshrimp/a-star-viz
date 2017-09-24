module Generate exposing (generateMaze, generateRandom)

import Types exposing (..)
import Random exposing (Generator)

generateMaze : List Coord
generateMaze = []

generateRandom : ( Int, Int ) -> Float -> Generator (List (List Tile))
generateRandom ( w, h ) probability = Random.list h (Random.list w (Random.map (\x -> if x < probability then Rock else E) (Random.float 0 1)))
