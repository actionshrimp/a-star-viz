module Main exposing (..)

import Types exposing (..)
import CalcPath
import Styles
import Dict exposing (Dict)
import Set
import Generate
import Random
import Time
import Tuple
import Html exposing (Html, text, div, img, h1, button, input, label)
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
        , gridToTerrain terrainGrid
        )


gridToTerrain : List (List Tile) -> Dict Coord Tile
gridToTerrain terrainGrid =
    terrainGrid
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


removeEndpoints : Model -> Dict Coord Tile -> Dict Coord Tile
removeEndpoints { start, goal } terrain =
    terrain
        |> Dict.update start (Maybe.map (always E))
        |> Dict.update goal (Maybe.map (always E))


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
          , terrainSize = terrainSize
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
          , showConnections = False
          }
        , Cmd.none
        )


---- UPDATE ----

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

        oldShowConnections =
            model.showConnections
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
                        |> update ResetProgress
                        |> Tuple.first
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

            Iterate am ->
                ( if not model.canIterate then
                    model
                  else if am == Auto && (not model.autoIterate) then
                           model
                  else
                    let
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
                            , autoIterate = if model.autoIterate then
                                                case am of
                                                    Auto -> True
                                                    Manual -> False
                                            else False
                        }
                , Cmd.none
                )

            ToggleAutoIterate ->
                ( { model | autoIterate = (not oldAutoIterate) }, Cmd.none )

            ToggleShowConnections ->
                ( { model | showConnections = (not oldShowConnections) }, Cmd.none )

            ResetProgress ->
                let
                    ( initial, _ ) =
                        init
                in
                    ( { model
                        | progress = initial.progress
                        , canIterate = initial.canIterate
                        , autoIterate = False
                        , path = initial.path
                      }
                    , Cmd.none
                    )

            ResetTerrain ->
                let
                    ( initial, _ ) =
                        init
                in
                    ( { model | terrain = initial.terrain }
                        |> update ResetProgress
                        |> Tuple.first
                    , Cmd.none
                    )

            GenerateRandomTerrain ->
                ( model, Random.generate UpdateTerrain (Generate.generateRandom terrainSize 0.3) )

            UpdateTerrain newTerrain ->
                ( { model
                    | terrain =
                        newTerrain
                            |> gridToTerrain
                            |> removeEndpoints model
                  }
                    |> update ResetProgress
                    |> Tuple.first
                , Cmd.none
                )



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
            [ ( model.goal, ("#" ++ Styles.complement4) )
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
                [ label [ class [ Styles.ToggleOption ] ]
                    [ input
                        [ HA.type_ "checkbox"
                        , E.onClick ToggleShowConnections
                        , HA.checked model.showConnections
                        ]
                        []
                    , Html.text "Show connections"
                    ]
                , button
                    [ (E.onClick ResetProgress)
                    ]
                    [ (Html.text "Reset progress") ]
                , button
                    [ (E.onClick ResetTerrain)
                    ]
                    [ (Html.text "Reset terrain") ]
                , button
                    [ (E.onClick GenerateRandomTerrain)
                    ]
                    [ (Html.text "Gen random terrain") ]
                , button
                    [ (E.onClick (Iterate Manual))
                    , (HA.disabled (not model.canIterate))
                    ]
                    [ (Html.text "Step") ]
                , button
                    [ (E.onClick ToggleAutoIterate)
                    , (HA.disabled (not model.canIterate))
                    ]
                    [ (Html.text
                        (if (model.canIterate && model.autoIterate) then
                            "Stop"
                         else
                            "Solve"
                        )
                      )
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if (model.canIterate && model.autoIterate) then
        Time.every (Time.millisecond * 2) (always (Iterate Auto))
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
