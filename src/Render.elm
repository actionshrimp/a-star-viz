module Render exposing (..)

import Dict exposing (Dict)
import Set
import Types exposing (..)
import Styles
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Html.Events as E


startAndGoal : Model -> List (Svg Msg)
startAndGoal model =
    let
        ( dx, dy ) =
            calcDeltas model
    in
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
            [ ( model.goal, ("#" ++ Styles.complement4) )
            ]


cellPoints : Model -> List (Svg a)
cellPoints model =
    let
        ( dx, dy ) =
            calcDeltas model

        terrainList =
            Dict.toList model.terrain
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


clickListeners : Model -> List (Svg Msg)
clickListeners model =
    let
        ( dx, dy ) =
            calcDeltas model

        terrainList =
            Dict.toList model.terrain
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
                        , if (model.dragging /= Nothing) then
                            [ E.onMouseEnter (MouseEnter ( x, y ))
                            , E.onMouseUp MouseUp
                            ]
                          else
                            []
                        ]
                    )
                    []
            )
            terrainList


rocks : Model -> List (Svg Msg)
rocks model =
    let
        ( dx, dy ) =
            calcDeltas model
    in
        model.terrain
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


connections : Model -> List (Svg Msg)
connections model =
    if not model.showConnections then
        []
    else
        List.concat
            [ model.progress.open |> Set.toList
            , model.progress.closed |> Set.toList
            ]
            |> List.filterMap
                (\c ->
                    let
                        ( x, y ) =
                            c

                        ( dx, dy ) =
                            calcDeltas model

                        cost =
                            Dict.get c model.progress.costs
                    in
                        cost
                            |> Maybe.andThen (\cost -> cost.parent)
                            |> Maybe.map
                                (\( px, py ) ->
                                    Svg.line
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
                )


pathConnections : Model -> List (Svg Msg)
pathConnections model =
    if not model.showConnections then
        []
    else
        model.path
            |> Maybe.map
                (\path ->
                    path
                        |> List.filterMap
                            (\c ->
                                let
                                    ( x, y ) =
                                        c

                                    ( dx, dy ) =
                                        calcDeltas model

                                    cost =
                                        Dict.get c model.progress.costs
                                in
                                    cost
                                        |> Maybe.andThen (\cost -> cost.parent)
                                        |> Maybe.map
                                            (\( px, py ) ->
                                                Svg.line
                                                    [ SA.x1 (toString (x * dx + dx // 2))
                                                    , SA.y1 (toString (y * dy + dy // 2))
                                                    , SA.x2 (toString (px * dx + dx // 2))
                                                    , SA.y2 (toString (py * dy + dy // 2))
                                                    , SA.stroke ("#" ++ Styles.complement4)
                                                    , SA.strokeWidth "4"
                                                    ]
                                                    []
                                            )
                            )
                )
            |> Maybe.withDefault []


progress : Model -> List (Svg Msg)
progress model =
    let
        ( dx, dy ) =
            calcDeltas model
    in
        List.concat
            [ model.progress.open
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
            , model.progress.closed
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
            , model.path
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
    in
        svg
            [ SA.width (toString w)
            , SA.height (toString h)
            ]
            (List.concat
                [ (startAndGoal model)
                , (progress model)
                , (connections model)
                , (pathConnections model)
                , (rocks model)
                , (cellPoints model)
                , (clickListeners model)
                ]
            )
