module Main exposing (..)

import Html exposing (Html, text, div, img, h1)
import Html.CssHelpers
import Styles


---- MODEL ----


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Model =
    { position : Coord
    , path : List Coord
    , size : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { position =
            { x = 0
            , y = 0
            }
      , path = []
      , size = 20
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


{ id, class, classList } =
    Html.CssHelpers.withNamespace Styles.ns


grid : Model -> Html Msg
grid model =
    div
        [ class [ Styles.Grid ] ]
        (List.repeat model.size
            (div [ class [ Styles.GridRow ] ]
                (List.repeat model.size
                    (div [ class [ Styles.GridCell ] ]
                        [ text "x" ]
                    )
                )
            )
        )


view : Model -> Html Msg
view model =
    div [ id [ Styles.Page ]]
        [ h1 [] [ (text "Pathfinder") ]
        , div [ class [ Styles.Container ]]
            [ grid model
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
