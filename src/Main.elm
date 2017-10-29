module Main exposing (..)

import Types exposing (..)
import CalcPath
import Styles
import Grid
import Render exposing (svgGrid)

import Dict exposing (Dict)
import Set
import Generate
import Random
import Time
import Html exposing (Html, text, div, img, h1, button, input, label)
import Html.Events as E
import Html.CssHelpers
import Html.Attributes as HA


---- MODEL ----


terrainSize : ( Int, Int )
terrainSize =
    ( 32, 32 )


terrain : Dict Coord Tile
terrain =
    Grid.emptyGrid terrainSize


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


resetProgress : Model -> Model
resetProgress model =
    let
        ( initial, _ ) =
            init
    in
        { model
            | progress = initial.progress
            , canIterate = initial.canIterate
            , autoIterate = False
            , path = initial.path
        }


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

                        autoIterate =
                            model.autoIterate
                                && case am of
                                    Auto ->
                                        True

                                    Manual ->
                                        False
                    in
                        { model
                            | canIterate = canIterate
                            , path = path
                            , progress = progress
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

            ResetTerrain ->
                let
                    ( initial, _ ) =
                        init
                in
                    ( { model | terrain = initial.terrain }
                        |> resetProgress
                    , Cmd.none
                    )

            GenerateRandomTerrain ->
                ( model, Random.generate UpdateTerrain (Generate.generateRandom terrainSize 0.3) )

            UpdateTerrain newTerrain ->
                ( { model
                    | terrain =
                        newTerrain
                            |> Grid.gridToTerrain
                            |> Grid.removeEndpoints model
                  }
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
