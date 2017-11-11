module Main exposing (..)

import Dict exposing (Dict)
import Grid
import Html exposing (Html, text, div, img, h1, button, input, label)
import Html.Attributes as HA
import Html.CssHelpers
import Html.Events as E
import Map
import Random
import Render exposing (svgGrid)
import Styles
import Time
import Types exposing (..)


---- MODEL ----


mapSize : ( Int, Int )
mapSize =
    ( 32, 32 )


init : ( Model, Cmd Msg )
init =
    let
        map =
            Map.emptyMap mapSize
    in
        ( { svgSize = ( 512, 512 )
          , dragging = Nothing
          , autoIterate = False
          , showConnections = False
          , map = map
          , grids =
                [ Grid.initGridState map

                --, Grid.initGridState map
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
            , grids = initial.grids
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
                        (Dict.get coord model.map.tiles)
                            |> Maybe.withDefault E
                            |> toggleTerrain
                in
                    ( { model | dragging = (Just targetTile) }
                        |> updateTiles (Dict.insert coord targetTile)
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
                            |> updateTiles (Dict.insert coord t)
                , Cmd.none
                )

            MouseUp ->
                ( { model
                    | dragging = Nothing
                  }
                , Cmd.none
                )

            Iterate am ->
                ( if not (List.any .canIterate model.grids) then
                    model
                  else if am == Auto && (not model.autoIterate) then
                    -- This happens when the timer subscription fires for the
                    -- last time or two after autoIterate has been turned off
                    model
                  else
                    let
                        autoIterate =
                            model.autoIterate
                                && case am of
                                    Auto ->
                                        True

                                    Manual ->
                                        False
                    in
                        { model
                            | grids = List.map (Grid.iterate model.map) model.grids
                            , autoIterate = autoIterate
                        }
                , Cmd.none
                )

            ToggleAutoIterate ->
                ( { model | autoIterate = (not oldAutoIterate) }, Cmd.none )

            ToggleShowConnections ->
                ( { model | showConnections = (not oldShowConnections) }, Cmd.none )

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
                        |> updateTiles (always initial.map.tiles)
                        |> resetProgress
                    , Cmd.none
                    )

            GenerateRandomTiles ->
                ( model, Random.generate UpdateTiles (Map.generateRandom mapSize 0.3) )

            UpdateTiles newTiles ->
                ( model
                    |> updateTiles
                        (always
                            (newTiles
                                |> Map.toTiles
                                |> Map.removeEndpoints model.map
                            )
                        )
                    |> resetProgress
                , Cmd.none
                )



---- VIEW ----


{ id, class, classList } =
    Html.CssHelpers.withNamespace Styles.ns


view : Model -> Html Msg
view model =
    div [ id [ Styles.Page ] ]
        [ div [ class [ Styles.Header ] ] [ (Html.text "Pathfinder") ]
        , div [ class [ Styles.HeaderRule ] ] []
        , div [ class [ Styles.Container ] ]
            [ div [ class [ Styles.Container ] ]
                (List.map
                    (svgGrid model)
                    model.grids
                )
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
