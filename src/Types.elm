module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type Hole
    = Hole


type Tile
    = E -- empty
    | Rock


type alias Coord =
    ( Int, Int )


type Parent
    = IsStart
    | Parent Coord


type alias Cost =
    { parent : Parent
    , heuristicScore : Float
    , walkScore : Float
    }


type alias GridState =
    { open : Set Coord
    , closed : Set Coord
    , costs : Dict Coord Cost
    , path : Maybe (List Coord)
    , canIterate : Bool
    , iteration : Int
    }


type alias GridDisplay =
    { current : GridState
    , rendered : GridState
    }


type alias Tiles =
    Dict Coord Tile


type alias Map =
    { start : Coord
    , goal : Coord
    , size : ( Int, Int )
    , tiles : Tiles
    , allowDiagonal : Bool
    }


type alias Model =
    { svgSize : ( Int, Int )
    , dragging : Maybe Tile
    , autoIterate : Bool
    , showConnections : Bool
    , gridDisplays : List GridDisplay
    , map : Map
    , renderEvery : Int
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
    | ToggleAllowDiagonal
    | ResetTiles
    | ResetProgress
    | ToggleShowConnections
    | GenerateRandomTiles
    | UpdateTiles (List (List Tile))


calcDeltas : Model -> ( Int, Int )
calcDeltas model =
    let
        ( w, h ) =
            model.svgSize

        ( rows, cols ) =
            model.map.size
    in
        ( w // rows
        , h // cols
        )


canIterate : Model -> Bool
canIterate m =
    List.any (\x -> x.rendered.canIterate) m.gridDisplays


updateTiles : (Tiles -> Tiles) -> Model -> Model
updateTiles f model =
    let
        oldMap =
            model.map

        oldTiles =
            model.map.tiles
    in
        { model | map = { oldMap | tiles = (f oldTiles) } }


toggleTerrain : Tile -> Tile
toggleTerrain t =
    case t of
        Rock ->
            E

        E ->
            Rock
