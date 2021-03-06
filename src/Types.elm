module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)


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
    , stats : Maybe GridStats
    }


type alias Tiles =
    { d : Dict Coord Tile
    , l : List ( ( Int, Int ), Tile )
    }


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
    | UpdateRenderEvery String
    | CalcRate Time


type alias GridStats =
    { lastCalcTime : Time
    , lastCalcIterations : Int
    , rate : Maybe Float
    }


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


updateTilesByCoord : (Dict Coord Tile -> Dict Coord Tile) -> Model -> Model
updateTilesByCoord f model =
    let
        oldMap =
            model.map

        oldTiles =
            model.map.tiles

        d =
            f oldTiles.d
    in
        { model
            | map =
                { oldMap
                    | tiles =
                        { d = d
                        , l = d |> Dict.toList
                        }
                }
        }


toggleTerrain : Tile -> Tile
toggleTerrain t =
    case t of
        Rock ->
            E

        E ->
            Rock


updateStats : Time -> GridDisplay -> GridDisplay
updateStats t gd =
    let
        its =
            gd.current.iteration
    in
        case gd.stats of
            Nothing ->
                { gd | stats = Just { lastCalcTime = t, lastCalcIterations = its, rate = Nothing } }

            Just s ->
                { gd
                    | stats =
                        Just
                            { lastCalcTime = t
                            , lastCalcIterations = its
                            , rate =
                                Just
                                    ((toFloat (its - s.lastCalcIterations))
                                        / (Time.inSeconds t - Time.inSeconds s.lastCalcTime)
                                    )
                            }
                }
