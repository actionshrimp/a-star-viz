module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text, div, img, h1)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Styles


---- MODEL ----


type alias Coord =
    ( Int, Int )


type alias Model =
    { terrain : Dict Coord Tile
    , position : Coord
    , path : List Coord
    , svgSize : ( Int, Int )
    }


type Tile
    = E -- empty
    | Rock


terrainGrid : List (List Tile)
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


terrainCols : Int
terrainCols =
    terrainGrid
        |> List.head
        |> Maybe.withDefault []
        |> List.length


terrainRows : Int
terrainRows =
    terrainGrid
        |> List.length


terrainDict : Dict Coord Tile
terrainDict =
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


init : ( Model, Cmd Msg )
init =
    ( { terrain = terrainDict
      , position = ( 0, 0 )
      , path = []
      , svgSize = ( 512, 512 )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = GridClick ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update (GridClick ( x, y )) model =
    let
        dummy = Debug.log "GridClick" ( x, y )
    in
        ( model, Cmd.none )



---- VIEW ----


{ id, class, classList } =
    Html.CssHelpers.withNamespace Styles.ns



-- divGrid : Model -> Html Msg
-- divGrid model =
--     let (w, h) = model.size in
--     div
--         [ class [ Styles.Grid ] ]
--         (List.repeat h
--             (div [ class [ Styles.GridRow ] ]
--                 (List.repeat model.size
--                     (div [ class [ Styles.GridCell ] ]
--                         [ Html.text "x" ]
--                     )
--                 )
--             )
--         )


calcDeltas : Model -> ( Int, Int )
calcDeltas model =
    let
        ( w, h ) =
            model.svgSize
    in
        ( w // terrainRows
        , h // terrainCols
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
                    [ Svg.Attributes.x (toString (x * dx))
                    , Svg.Attributes.y (toString (y * dy))
                    , width (toString dx)
                    , height (toString dy)
                    , fill "red"
                    , fillOpacity "0"
                    , onClick (GridClick ( x, y ))
                    ]
                    []
            )
            terrainList


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
