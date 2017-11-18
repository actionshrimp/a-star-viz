module Main exposing (..)

import Dict exposing (Dict)
import Grid
import Html exposing (Html, text, div, img, h1, button, input, label)
import Html.Attributes as HA
import Html.Events as E
import Map
import Random
import Render exposing (svgGrid)
import Styles exposing (class, id)
import Time
import Types exposing (..)


---- MODEL ----


mapSize : ( Int, Int )
mapSize =
    ( 48, 48 )


inputToRenderEvery : String -> Int
inputToRenderEvery i =
    (String.toInt i)
        |> Result.withDefault 0
        |> (^) 10


renderEveryToInput : Int -> String
renderEveryToInput i =
    logBase 10 (toFloat i)
        |> round
        |> toString


init : ( Model, Cmd Msg )
init =
    let
        map =
            Map.emptyMap mapSize

        g =
            Grid.initGridState map
    in
        ( { svgSize = ( 512, 512 )
          , dragging = Nothing
          , autoIterate = False
          , showConnections = False
          , map = map
          , renderEvery = 1
          , gridDisplays =
                [ { current = g, rendered = g, stats = Nothing }
                ]
          }
        , Cmd.none
        )



---- UPDATE ----


resetProgress : Model -> Model
resetProgress model =
    let
        ( initial, _ ) =
            init
    in
        { model
            | autoIterate = False
            , gridDisplays = initial.gridDisplays
        }


updateGridDisplay : Model -> GridDisplay -> GridDisplay
updateGridDisplay model gd =
    let
        n =
            model.renderEvery

        next =
            Grid.iterate model.map gd.current
    in
        if
            (not next.canIterate)
                || (next.iteration - gd.rendered.iteration)
                >= model.renderEvery
        then
            { gd
                | rendered = next
                , current = next
            }
        else
            { gd
                | rendered = gd.rendered
                , current = next
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldAutoIterate =
            model.autoIterate

        oldShowConnections =
            model.showConnections

        oldMap =
            model.map
    in
        case msg of
            MouseDown coord ->
                let
                    targetTile =
                        (Dict.get coord model.map.tiles.d)
                            |> Maybe.withDefault E
                            |> toggleTerrain
                in
                    ( { model | dragging = (Just targetTile) }
                        |> updateTilesByCoord (Dict.insert coord targetTile)
                        |> update ResetProgress
                        |> Tuple.first
                    , Cmd.none
                    )

            MouseEnter coord ->
                ( case model.dragging of
                    Nothing ->
                        model

                    Just t ->
                        model
                            |> updateTilesByCoord (Dict.insert coord t)
                , Cmd.none
                )

            MouseUp ->
                ( { model
                    | dragging = Nothing
                  }
                , Cmd.none
                )

            Iterate am ->
                ( if am == Auto && (not model.autoIterate) then
                    -- This happens when the timer subscription fires for the
                    -- last time or two after autoIterate has been turned off
                    model
                  else
                    let
                        autoIterate =
                            model.autoIterate && am == Auto
                    in
                        { model
                            | gridDisplays =
                                model.gridDisplays
                                    |> List.map (updateGridDisplay model)
                            , autoIterate = autoIterate
                        }
                , Cmd.none
                )

            ToggleAutoIterate ->
                ( { model | autoIterate = (not oldAutoIterate) }, Cmd.none )

            ToggleShowConnections ->
                ( { model | showConnections = (not oldShowConnections) }, Cmd.none )

            ToggleAllowDiagonal ->
                let
                    map =
                        model.map
                in
                    ( { model | map = { map | allowDiagonal = not model.map.allowDiagonal } }
                        |> resetProgress
                    , Cmd.none
                    )

            ResetProgress ->
                ( resetProgress model
                , Cmd.none
                )

            ResetTiles ->
                let
                    ( initial, _ ) =
                        init
                in
                    ( model
                        |> updateTilesByCoord (always initial.map.tiles.d)
                        |> resetProgress
                    , Cmd.none
                    )

            GenerateRandomTiles ->
                ( model, Random.generate UpdateTiles (Map.generateRandom mapSize 0.3) )

            UpdateTiles newTiles ->
                let
                    oldMap =
                        model.map

                    newMap =
                        { oldMap
                            | tiles =
                                newTiles
                                    |> Map.toTiles
                                    |> Map.removeEndpoints oldMap
                        }
                in
                    ( { model | map = newMap }
                        |> resetProgress
                    , Cmd.none
                    )

            UpdateRenderEvery reStr ->
                ( { model
                    | renderEvery = inputToRenderEvery reStr
                  }
                , Cmd.none
                )

            CalcRate newTime ->
                ( { model | gridDisplays = List.map (updateStats newTime) model.gridDisplays }
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id [ Styles.Page ] ]
        [ div [ class [ Styles.Header ] ] [ (Html.text "Pathfinder") ]
        , div [ class [ Styles.HeaderRule ] ] []
        , div [ class [ Styles.Container ] ]
            [ div [ class [ Styles.Container ] ]
                (List.map (svgGrid model) model.gridDisplays)
            , div [ class [ Styles.Sidebar ] ]
                [ div [ class [ Styles.SidebarHeading ] ]
                    [ Html.text "Options:" ]
                , label [ class [ Styles.ToggleOption ] ]
                    [ input
                        [ HA.type_ "checkbox"
                        , E.onClick ToggleAllowDiagonal
                        , HA.checked model.map.allowDiagonal
                        ]
                        []
                    , Html.text "Allow diagonal movement"
                    ]
                , div [ class [ Styles.SidebarHeading ] ]
                    [ Html.text "Rendering:" ]
                , label [ class [ Styles.ToggleOption ] ]
                    [ input
                        [ HA.type_ "checkbox"
                        , E.onClick ToggleShowConnections
                        , HA.checked model.showConnections
                        ]
                        []
                    , Html.text "Show connections"
                    ]
                , label [ class [ Styles.SliderInput ] ]
                    [ input
                        [ HA.type_ "range"
                        , HA.min "0"
                        , HA.max "5"
                        , E.onInput UpdateRenderEvery
                        , HA.value (renderEveryToInput model.renderEvery)
                        ]
                        []
                    , Html.text
                        (if model.renderEvery == 1 then
                            "Render every iteration"
                         else
                            ("Render every " ++ (toString model.renderEvery) ++ " iterations")
                        )
                    ]
                , div [ class [ Styles.SidebarHeading ] ]
                    [ Html.text "Actions:" ]
                , button
                    [ (E.onClick ResetProgress)
                    ]
                    [ (Html.text "Reset progress") ]
                , button
                    [ (E.onClick ResetTiles)
                    ]
                    [ (Html.text "Reset tiles") ]
                , button
                    [ (E.onClick GenerateRandomTiles)
                    ]
                    [ (Html.text "Gen random terrain") ]
                , button
                    [ (E.onClick (Iterate Manual))
                    , (HA.disabled (not (canIterate model)))
                    ]
                    [ (Html.text "Step") ]
                , button
                    [ (E.onClick ToggleAutoIterate)
                    , (HA.disabled (not (canIterate model)))
                    ]
                    [ (Html.text
                        (if (canIterate model && model.autoIterate) then
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
    if (canIterate model && model.autoIterate) then
        Sub.batch
            [ Time.every (Time.millisecond * 2) (always (Iterate Auto))
            , Time.every (Time.second) CalcRate
            ]
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
