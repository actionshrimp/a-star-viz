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

type alias HeuristicCost = Float

-- Nothing = not walkable
-- Just x = walkable with cost x
type alias WalkCost = Maybe Float

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
    , path : Maybe (List Coord)
    , svgSize : ( Int, Int )
    , dragging : Maybe Tile
    , progress : Iteration
    , canIterate : Bool
    , autoIterate : Bool
    , showConnections : Bool
    , terrainSize : ( Int, Int )
    }

type AutoManual
    = Auto
    | Manual


type Msg
    = MouseDown ( Int, Int )
    | MouseEnter ( Int, Int )
    | MouseUp
    | Iterate AutoManual
    | ToggleAutoIterate
    | ResetTerrain
    | ResetProgress
    | ToggleShowConnections
    | GenerateRandomTerrain
    | UpdateTerrain (List (List Tile))

calcDeltas : Model -> ( Int, Int )
calcDeltas model =
    let
        ( w, h ) =
            model.svgSize

        ( rows, cols ) =
            model.terrainSize
    in
        ( w // rows
        , h // cols
        )
