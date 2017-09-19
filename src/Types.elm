module Types exposing (..)

import Dict exposing (Dict)

type Tile
    = E -- empty
    | Rock

type alias Coord =
    ( Int, Int )

type alias Terrain =
    Dict Coord Tile

