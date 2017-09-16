module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)


ns : String
ns =
    "asv"


type CssClasses
    = Grid
    | GridRow
    | GridCell
    | Container


type CssIds
    = Page


css : Stylesheet
css =
    (stylesheet << namespace ns)
        [ body
            [ fontFamilies
                [ (qt "Source Sans Pro")
                , (qt "Trebuchet MS")
                , (qt "Lucidia Grande")
                , (qt "Bitstream Vera Sans")
                , (qt "Helvetica Neue")
                , "sans-serif"
                ]
            , fontSize (px 30)
            , margin (px 0)
            , textAlign center
            , color (hex "293c4b")
            ]
        , id Page
            [ width (pct 100)
            ]
        , class Container
            [ property "display" "flex"
            , justifyContent spaceAround
            ]
        , class Grid
            [ property "display" "flex"
            , flexDirection column
            ]
        , class GridRow
            [ property "display" "flex"
            , flexDirection row
            ]
        , class GridCell
            [ width (px 50)
            , height (px 50)
            ]
        ]
