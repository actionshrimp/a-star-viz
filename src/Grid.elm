module Grid exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Tuple
import Types exposing (..)


totalScore : Cost -> Float
totalScore { walkScore, heuristicScore } =
    walkScore + heuristicScore


neighbours : Map -> Coord -> List Coord
neighbours map ( x, y ) =
    List.concat
        [ [ ( x + 1, y )
          , ( x - 1, y )
          , ( x, y + 1 )
          , ( x, y - 1 )
          ]
        , if map.allowDiagonal then
            [ ( x + 1, y + 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y - 1 )
            ]
          else
            []
        ]
        |> List.filter
        |> (flip Dict.member) map.tiles.d


tileCost : Tile -> Maybe Float
tileCost t =
    case t of
        E ->
            Just 1

        Rock ->
            Nothing


heuristicScore : Coord -> Coord -> Float
heuristicScore goal c =
    let
        ( gx, gy ) =
            goal

        ( x, y ) =
            c
    in
        ((gx - x) ^ 2 + (gy - y) ^ 2)
            |> toFloat
            |> sqrt
            |> (*) 10


travelCost : Map -> ( Parent, Cost ) -> Coord -> Maybe Float
travelCost map ( parentCoord, parentCost ) ( x, y ) =
    case parentCoord of
        IsStart ->
            Nothing

        Parent ( px, py ) ->
            Dict.get ( x, y ) map.tiles.d
                |> Maybe.andThen tileCost
                |> Maybe.map
                    (\tc ->
                        (parentCost.walkScore
                            + (((px - x) ^ 2 + (py - y) ^ 2)
                                |> toFloat
                                |> sqrt
                                |> (*) 10
                              )
                        )
                    )


calcCost : Map -> ( Parent, Cost ) -> Coord -> Maybe ( Coord, Cost )
calcCost map ( parentC, parentCost ) c =
    travelCost map ( parentC, parentCost ) c
        |> Maybe.map
            (\tc ->
                ( c
                , { parent = parentC
                  , heuristicScore = heuristicScore map.goal c
                  , walkScore = tc
                  }
                )
            )


startCost : Coord -> Coord -> Cost
startCost goal start =
    { parent = IsStart
    , heuristicScore = heuristicScore goal start
    , walkScore = 0
    }


mergeCosts : Dict Coord Cost -> List ( Coord, Cost ) -> Dict Coord Cost
mergeCosts costs newCosts =
    let
        mergeCost ( c, newCost ) costs =
            Dict.update c
                (\old ->
                    case old of
                        Nothing ->
                            Just newCost

                        Just oldCost ->
                            if (totalScore oldCost) < (totalScore newCost) then
                                Just oldCost
                            else
                                Just newCost
                )
                costs
    in
        List.foldl mergeCost costs newCosts


tracePathRec : Coord -> GridState -> List Coord -> Maybe (List Coord)
tracePathRec coord gs coords =
    Dict.get coord gs.costs
        |> Maybe.andThen
            (\c ->
                case c.parent of
                    IsStart ->
                        Just coords

                    Parent pc ->
                        tracePathRec pc gs (pc :: coords)
            )


tracePath : Map -> GridState -> Maybe (List Coord)
tracePath map gs =
    tracePathRec map.goal gs [ map.goal ]


initGridState : Map -> GridState
initGridState { start, goal } =
    { path = Nothing
    , open = Set.fromList ([ start ])
    , costs = Dict.fromList [ ( start, startCost goal start ) ]
    , closed = Set.empty
    , canIterate = True
    , iteration = 0
    }


bestOpen : Set Coord -> Dict Coord Cost -> Maybe ( Coord, Cost )
bestOpen open costs =
    -- definitely not the most efficient way of doing this but does the job for my purposes!
    costs
        |> Dict.toList
        |> List.filter (\( c, cost ) -> Set.member c open)
        |> List.map (\( c, cost ) -> ( c, cost, (totalScore cost) ))
        |> List.sortBy (\( c, cost, total ) -> total)
        |> List.head
        |> Maybe.map (\( c, cost, total ) -> ( c, cost ))


iterate : Map -> GridState -> GridState
iterate map gs =
    if not gs.canIterate then
        gs
    else
        let
            newCosts ( best, bestCost ) =
                (neighbours map best)
                    |> List.filter (\c -> not (Set.member c gs.closed))
                    |> List.filterMap (calcCost map ( Parent best, bestCost ))

            withPath gs =
                if (Set.member map.goal gs.closed) then
                    { gs
                        | path = (tracePath map gs)
                        , canIterate = False
                    }
                else
                    gs

            next ( best, newCosts ) =
                { open =
                    gs.open
                        |> Set.remove best
                        |> Set.union (Set.fromList (List.map Tuple.first newCosts))
                , closed = Set.insert best gs.closed
                , costs = mergeCosts gs.costs newCosts
                , path = gs.path
                , canIterate = True
                , iteration = gs.iteration + 1
                }
                    |> withPath
        in
            bestOpen gs.open gs.costs
                |> Maybe.map (\( best, bestCost ) -> ( best, newCosts ( best, bestCost ) ))
                |> Maybe.map next
                |> Maybe.withDefault { gs | canIterate = False }
