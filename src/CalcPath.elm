module CalcPath exposing (iterate, startCost, costValue)

import Types exposing (..)
import Tuple
import Dict exposing (Dict)
import Set exposing (Set)


costValue : Cost -> Maybe Float
costValue cost =
    Maybe.map (\x -> x + cost.heuristicRemainingCost) cost.travelCost


bestOpen : Set Coord -> Dict Coord Cost -> Maybe Coord
bestOpen open costs =
    -- definitely not the most efficient way of doing this but does the job for my purposes!
    costs
        |> Dict.toList
        |> List.filter (\( c, cost ) -> Set.member c open)
        |> List.filterMap
            (\( c, cost ) ->
                costValue cost
                    |> Maybe.map (\total -> ( c, total, cost.heuristicRemainingCost ))
            )
        |> List.sortBy (\( c, total, hc ) -> (round total, hc))
        |> List.head
        |> Maybe.map (\( c, _, hc ) -> c)

neighbours : Terrain -> Coord -> List Coord
neighbours terrain ( x, y ) =
    [ ( x + 1, y )
    , ( x + 1, y + 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x - 1, y - 1 )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]
        |> List.filter (\c -> Dict.get c terrain /= Nothing)


tileCost : Tile -> WalkCost
tileCost t =
    case t of
        E ->
            Just 1

        Rock ->
            Nothing


heuristicRemainingCost : Coord -> Coord -> Float
heuristicRemainingCost goal c =
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


travelCost : Terrain -> ( Maybe Coord, Cost ) -> Coord -> WalkCost
travelCost terrain ( parentCoord, parentCost ) ( x, y ) =
    let
        _ =
            Debug.log "parentCoord" parentCoord
    in
        Dict.get ( x, y ) terrain
            |> Maybe.andThen tileCost
            |> Maybe.map3
                (\ptc ( px, py ) tc ->
                    (ptc
                        + (((px - x) ^ 2 + (py - y) ^ 2)
                            |> toFloat
                            |> sqrt
                            |> (*) 10
                          )
                    )
                )
                parentCost.travelCost
                parentCoord


calcCost : Model -> ( Maybe Coord, Cost ) -> Coord -> ( Coord, Cost )
calcCost model ( parentC, parentCost ) c =
    ( c
    , { parent = parentC
      , heuristicRemainingCost = heuristicRemainingCost model.goal c
      , travelCost = travelCost model.terrain ( parentC, parentCost ) c
      }
    )


startCost : Coord -> Coord -> Cost
startCost goal start =
    { parent = Nothing
    , heuristicRemainingCost = heuristicRemainingCost goal start
    , travelCost = Just 0
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
                            case ( (costValue oldCost), (costValue newCost) ) of
                                ( Nothing, Nothing ) ->
                                    Nothing

                                ( Nothing, Just n ) ->
                                    Just newCost

                                ( Just o, Nothing ) ->
                                    Just oldCost

                                ( Just o, Just n ) ->
                                    if o < n then
                                        Just oldCost
                                    else
                                        Just newCost
                )
                costs
    in
        List.foldl mergeCost costs newCosts


iterate : Model -> Iteration -> Maybe Iteration
iterate model it =
    let
        { open, closed, costs } =
            it

        best =
            bestOpen open costs

        newCosts best =
            Dict.get best costs
                |> Maybe.map
                    (\cost ->
                        (neighbours model.terrain best)
                            |> List.filter (\c -> not (Set.member c closed))
                            |> List.map (calcCost model ( Just best, cost ))
                    )
                |> Maybe.map (\costs -> ( best, costs ))

        newIter ( best, newCosts ) =
            { open =
                open
                    |> Set.remove best
                    |> Set.union (Set.fromList (List.map Tuple.first newCosts))
            , closed = Set.insert best closed
            , costs = mergeCosts costs newCosts
            }
    in
        best
            |> Maybe.andThen newCosts
            |> Maybe.map newIter
