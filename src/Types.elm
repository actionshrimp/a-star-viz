module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type Hole = Hole

type Tile
    = E -- empty
    | Rock

type alias Coord =
    ( Int, Int )

type alias Terrain =
    Dict Coord Tile

type alias HeuristicCost = Int

-- Nothing = not walkable
-- Just x = walkable with cost x
type alias WalkCost = Maybe Int

type alias Cost =
    { parent: Maybe Coord
    , heuristicRemainingCost: HeuristicCost
    , travelCost: WalkCost
    }

type alias Iteration =
    { open: Set Coord
    , closed: Set Coord
    , costs: Dict Coord Cost
    }

type alias Model =
    { terrain : Terrain
    , start : Coord
    , goal : Coord
    , path : List Coord
    , svgSize : ( Int, Int )
    , dragging : Maybe Tile
    , progress : Iteration
    , canIterate : Bool
    }

