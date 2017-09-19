module Main exposing (..)

import Types exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, text, div, img, h1)
import Html.Events as E
import Html.CssHelpers
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Styles


---- MODEL ----


type alias Model =
    { terrain : Terrain
    , position : Coord
    , path : List Coord
    , svgSize : ( Int, Int )
    , dragging : Maybe Tile
    }


( terrainSize, terrain ) =
    let
        terrainGrid =
            [ [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            , [ E, E, E, E, E, E, E, E, E, E ]
            ]

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
    ( { terrain = terrain
      , position = ( 0, 0 )
      , path = []
      , svgSize = ( 512, 512 )
      , dragging = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = MouseDown ( Int, Int )
    | MouseEnter ( Int, Int )
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleTerrain t =
            case t of
                Rock ->
                    E

                E ->
                    Rock
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
                        [ [ Svg.Attributes.x (toString (x * dx))
                          , Svg.Attributes.y (toString (y * dy))
                          , width (toString dx)
                          , height (toString dy)
                          , fill "red"
                          , fillOpacity "0"
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
                    Svg.circle
                        [ Svg.Attributes.cx (toString (x * dx + dx // 2))
                        , Svg.Attributes.cy (toString (y * dy + dy // 2))
                        , r (toString (dx // 2))
                        , fill "red"
                        ]
                        []
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
                    [ Svg.Attributes.cx (toString (x * dx + dx // 2))
                    , Svg.Attributes.cy (toString (y * dy + dy // 2))
                    , r "2"
                    , fill "black"
                    ]
                    []
            )
            terrainList


svgGrid : Model -> Svg Msg
svgGrid model =
    let
        ( w, h ) =
            model.svgSize
    in
        svg
            [ width (toString w)
            , height (toString h)
            ]
            (List.concat
                [ (cellPoints model)
                , (rocks model)
                , (clickListeners model)
                ]
            )


view : Model -> Html Msg
view model =
    div [ id [ Styles.Page ] ]
        [ h1 [] [ (Html.text "Pathfinder") ]
        , div [ class [ Styles.Container ] ]
            [ svgGrid model
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
