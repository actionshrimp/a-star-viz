module Render exposing (..)

import Dict exposing (Dict)
import Html.Events as E
import Set
import Styles
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Types exposing (..)


startAndGoal : ( Int, Int ) -> Map -> List (Svg Msg)
startAndGoal ( dx, dy ) map =
    List.map
        (\( ( x, y ), color ) ->
            Svg.rect
                [ SA.x (toString (x * dx))
                , SA.y (toString (y * dy))
                , SA.width (toString dx)
                , SA.height (toString dy)
                , SA.fill color
                ]
                []
        )
        [ ( map.goal, ("#" ++ Styles.complement4) )
        ]


cellPoints : ( Int, Int ) -> Map -> List (Svg a)
cellPoints ( dx, dy ) map =
    let
        terrainList =
            Dict.toList map.tiles
    in
        List.map
            (\( ( x, y ), tile ) ->
                Svg.circle
                    [ SA.cx (toString (x * dx + dx // 2))
                    , SA.cy (toString (y * dy + dy // 2))
                    , SA.r (toString (max (dx // 20) 1))
                    , SA.fill ("#" ++ Styles.grey)
                    , SA.fillOpacity "0.2"
                    ]
                    []
            )
            terrainList


clickListeners : ( Int, Int ) -> Model -> List (Svg Msg)
clickListeners ( dx, dy ) model =
    let
        terrainList =
            Dict.toList model.map.tiles
    in
        List.map
            (\( ( x, y ), tile ) ->
                Svg.rect
                    (List.concat
                        [ [ SA.x (toString (x * dx))
                          , SA.y (toString (y * dy))
                          , SA.width (toString dx)
                          , SA.height (toString dy)
                          , SA.fillOpacity "0"
                          , E.onMouseDown (MouseDown ( x, y ))
                          ]
                        , case model.dragging of
                            Nothing ->
                                []

                            Just _ ->
                                [ E.onMouseEnter (MouseEnter ( x, y ))
                                , E.onMouseUp MouseUp
                                ]
                        ]
                    )
                    []
            )
            terrainList


rocks : ( Int, Int ) -> Map -> List (Svg Msg)
rocks ( dx, dy ) map =
    map.tiles
        |> Dict.toList
        |> List.filter (\( c, t ) -> t == Rock)
        |> List.map
            (\( ( x, y ), tile ) ->
                Svg.rect
                    [ SA.x (toString (x * dx))
                    , SA.y (toString (y * dy))
                    , SA.width (toString dx)
                    , SA.height (toString dy)
                    , SA.fill ("#" ++ Styles.secY0)
                    , SA.stroke ("#" ++ Styles.secY3)
                    ]
                    []
            )


connections : ( Int, Int ) -> GridState -> List (Svg Msg)
connections ( dx, dy ) gs =
    let
        render ( x, y ) p =
            case p of
                IsStart ->
                    Nothing

                Parent ( px, py ) ->
                    Just
                        (Svg.line
                            [ SA.x1 (toString (x * dx + dx // 2))
                            , SA.y1 (toString (y * dy + dy // 2))
                            , SA.x2 (toString (px * dx + dx // 2))
                            , SA.y2 (toString (py * dy + dy // 2))
                            , SA.stroke ("#" ++ Styles.secX4)
                            , SA.strokeOpacity "0.5"
                            , SA.strokeWidth "4"
                            ]
                            []
                        )
    in
        List.concat
            [ gs.open |> Set.toList
            , gs.closed |> Set.toList
            ]
            |> List.filterMap
                (\c ->
                    Dict.get c gs.costs
                        |> Maybe.map .parent
                        |> Maybe.andThen (render c)
                )


pathConnections : ( Int, Int ) -> GridState -> List (Svg Msg)
pathConnections ( dx, dy ) gs =
    let
        render ( x, y ) p =
            case p of
                IsStart ->
                    Nothing

                Parent ( px, py ) ->
                    Just
                        (Svg.line
                            [ SA.x1 (toString (x * dx + dx // 2))
                            , SA.y1 (toString (y * dy + dy // 2))
                            , SA.x2 (toString (px * dx + dx // 2))
                            , SA.y2 (toString (py * dy + dy // 2))
                            , SA.stroke ("#" ++ Styles.complement4)
                            , SA.strokeWidth "4"
                            ]
                            []
                        )
    in
        gs.path
            |> Maybe.withDefault []
            |> (List.filterMap
                    (\c ->
                        Dict.get c gs.costs
                            |> Maybe.map .parent
                            |> Maybe.andThen (render c)
                    )
               )


progress : ( Int, Int ) -> GridState -> List (Svg Msg)
progress ( dx, dy ) gs =
    List.concat
        [ gs.open
            |> Set.toList
            |> List.map
                (\( x, y ) ->
                    Svg.rect
                        [ SA.x (toString (x * dx))
                        , SA.y (toString (y * dx))
                        , SA.width (toString dx)
                        , SA.height (toString dy)
                        , SA.fill ("#" ++ Styles.secX1)
                        , SA.stroke ("#" ++ Styles.secX2)
                        ]
                        []
                )
        , gs.closed
            |> Set.toList
            |> List.map
                (\( x, y ) ->
                    Svg.rect
                        [ SA.x (toString (x * dx))
                        , SA.y (toString (y * dx))
                        , SA.width (toString dx)
                        , SA.height (toString dy)
                        , SA.fill ("#" ++ Styles.secX0)
                        , SA.stroke ("#" ++ Styles.secX3)
                        ]
                        []
                )
        , gs.path
            |> Maybe.withDefault []
            |> List.map
                (\( x, y ) ->
                    Svg.rect
                        [ SA.x (toString (x * dx))
                        , SA.y (toString (y * dx))
                        , SA.width (toString dx)
                        , SA.height (toString dy)
                        , SA.fill ("#" ++ Styles.complement2)
                        , SA.stroke ("#" ++ Styles.complement3)
                        ]
                        []
                )
        ]


svgGrid : Model -> Svg Msg
svgGrid model =
    let
        ( w, h ) =
            model.svgSize

        deltas =
            calcDeltas model
    in
        svg
            [ SA.width (toString w)
            , SA.height (toString h)
            ]
            (List.concat
                [ (startAndGoal deltas model.map)
                , (List.concatMap (progress deltas) model.grids)
                , (rocks deltas model.map)
                , (if model.showConnections then
                    List.concatMap (connections deltas) model.grids
                   else
                    []
                  )
                , (if model.showConnections then
                    List.concatMap (pathConnections deltas) model.grids
                   else
                    []
                  )
                , (cellPoints deltas model.map)
                , (clickListeners deltas model)
                ]
            )
