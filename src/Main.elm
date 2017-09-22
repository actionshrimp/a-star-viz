module Main exposing (..)

import Types exposing (..)
import CalcPath
import Styles
import Dict exposing (Dict)
import Set
import Time
import Html exposing (Html, text, div, img, h1, button)
import Html.Events as E
import Html.CssHelpers
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes as SA


---- MODEL ----


( terrainSize, terrain ) =
    let
        ( w, h ) =
            ( 32, 32 )

        terrainGrid =
            List.repeat h (List.repeat w E)

        cols =
            terrainGrid
                |> List.head
                |> Maybe.withDefault []
                |> List.length

        rows =
            terrainGrid
                |> List.length
    in
        ( ( rows, cols )
        , (terrainGrid
            |> List.indexedMap
                (\y row ->
                    List.indexedMap
                        (\x tile ->
                            ( ( x, y ), tile )
                        )
                        row
                )
            |> List.concat
            |> Dict.fromList
          )
        )


init : ( Model, Cmd Msg )
init =
    let
        ( rows, cols ) =
            terrainSize

        start =
            ( 0, 0 )

        goal =
            ( rows - 1, cols - 1 )
    in
        ( { terrain = terrain
          , start = start
          , goal = goal
          , path = Nothing
          , svgSize = ( 512, 512 )
          , dragging = Nothing
          , progress =
                { open = Set.fromList ([ start ])
                , costs = Dict.fromList [ ( start, CalcPath.startCost goal start ) ]
                , closed = Set.empty
                }
          , canIterate = True
          , autoIterate = False
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = MouseDown ( Int, Int )
    | MouseEnter ( Int, Int )
    | MouseUp
    | Iterate
    | ToggleAutoIterate
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleTerrain t =
            case t of
                Rock ->
                    E

                E ->
                    Rock

        oldAutoIterate =
            model.autoIterate
    in
        case msg of
            MouseDown coord ->
                let
                    targetTile =
                        (Dict.get coord model.terrain)
                            |> Maybe.withDefault E
                            |> toggleTerrain
                in
                    ( { model
                        | terrain = (Dict.insert coord targetTile model.terrain)
                        , dragging = (Just targetTile)
                      }
                    , Cmd.none
                    )

            MouseEnter coord ->
                ( case model.dragging of
                    Nothing ->
                        model

                    Just t ->
                        { model | terrain = (Dict.insert coord t model.terrain) }
                , Cmd.none
                )

            MouseUp ->
                ( { model
                    | dragging = Nothing
                  }
                , Cmd.none
                )

            Iterate ->
                ( let
                    next =
                        CalcPath.iterate model model.progress

                    path =
                        next |> Maybe.andThen (CalcPath.hasPath model)

                    canIterate =
                        next /= Nothing && path == Nothing

                    progress =
                        Maybe.withDefault model.progress next
                  in
                    { model
                        | canIterate = canIterate
                        , path = path
                        , progress = progress
                    }
                , Cmd.none
                )

            ToggleAutoIterate ->
                ( { model | autoIterate = (not oldAutoIterate) }, Cmd.none )

            Reset ->
                init



---- VIEW ----


{ id, class, classList } =
    Html.CssHelpers.withNamespace Styles.ns


calcDeltas : Model -> ( Int, Int )
calcDeltas model =
    let
        ( w, h ) =
            model.svgSize

        ( rows, cols ) =
            terrainSize
    in
        ( w // rows
        , h // cols
        )


cellPoints : Model -> List (Svg Msg)
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
            [ ( model.goal, ("#" ++ Styles.complement0) )
            ]


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
                            , SA.fillOpacity "0"
                            , SA.stroke ("#" ++ Styles.secX1)
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
                            , SA.fill ("#" ++ Styles.primary0)
                            , SA.stroke ("#" ++ Styles.primary3)
                            ]
                            []
                    )

            --            , model.progress.costs
            --                |> Dict.toList
            --                |> List.concatMap
            --                    (\( ( x, y ), cost ) ->
            --                        [ Svg.text_
            --                            [ SA.x (toString (x * dx + 5))
            --                            , SA.y (toString (y * dy + 15))
            --                            , SA.style "font-size: 10px"
            --                            ]
            --                            [ Svg.text
            --                                (case cost.travelCost of
            --                                    Nothing ->
            --                                        "-"
            --
            --                                    Just x ->
            --                                     (toString (round x))
            --                                )
            --                            ]
            --                        , Svg.text_
            --                            [ SA.x (toString (x * dx + dx - 15))
            --                            , SA.y (toString (y * dy + 15))
            --                            , SA.style "font-size: 10px"
            --                            ]
            --                            [ Svg.text (toString (round cost.heuristicRemainingCost)) ]
            --                        , Svg.text_
            --                            [ SA.x (toString (x * dx + 5))
            --                            , SA.y (toString (y * dy + dy - 5))
            --                            , SA.style "font-size: 10px"
            --                            ]
            --                            [ Svg.text
            --                                (case (CalcPath.costValue cost) of
            --                                    Nothing ->
            --                                        "-"
            --
            --                                    Just x ->
            --                                        (toString (round x))
            --                                )
            --                            ]
            --                        ]
            --                    )
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
                [ (progress model)
                , (startAndGoal model)
                , (rocks model)
                , (cellPoints model)
                , (clickListeners model)
                ]
            )


view : Model -> Html Msg
view model =
    div [ id [ Styles.Page ] ]
        [ div [ class [ Styles.Header ] ] [ (Html.text "Pathfinder") ]
        , div [ class [ Styles.HeaderRule ] ] []
        , div [ class [ Styles.Container ] ]
            [ div [ class [ Styles.Container ] ]
                [ svgGrid model
                ]
            , div [ class [ Styles.Sidebar ] ]
                [ button
                    [ (E.onClick Reset)
                    ]
                    [ (Html.text "Reset") ]
                , button
                    [ (E.onClick Iterate)
                    , (HA.disabled (not model.canIterate))
                    ]
                    [ (Html.text "Iterate") ]
                , button
                    [ (E.onClick ToggleAutoIterate)
                    , (HA.disabled (not model.canIterate))
                    ]
                    [ (Html.text
                        (if (model.canIterate && model.autoIterate) then
                            "Stop Auto"
                         else
                            "Auto"
                        )
                      )
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if (model.canIterate && model.autoIterate) then
        Time.every (Time.millisecond * 2) (always Iterate)
    else
        Sub.none



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
