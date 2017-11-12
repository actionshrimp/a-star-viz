module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, button, input)
import Css.Namespace exposing (namespace)


ns : String
ns =
    "asv"


type CssClasses
    = Grid
    | GridRow
    | GridCell
    | Container
    | Sidebar
    | Header
    | HeaderRule
    | ToggleOption
    | SliderInput
    | SidebarHeading


type CssIds
    = Page



--#####  Color Palette by Paletton.com
--#####  Palette URL: http://paletton.com/#uid=60u0u0km4KpbqVchjPSqBE-zJzx
--
--


grey =
    "333333"



--*** Primary color:


primary0 =
    "FF9F4F"


primary1 =
    "FFCDA4"


primary2 =
    "FFB475"


primary3 =
    "FF8B2B"


primary4 =
    "FF7400"



--*** Secondary color (1):


secX0 =
    "FFC44F"


secX1 =
    "FFE1A4"


secX2 =
    "FFD175"


secX3 =
    "FFB82B"


secX4 =
    "FFAA00"



--*** Secondary color (2):


secY0 =
    "FF4F4F"


secY1 =
    "FFA4A4"


secY2 =
    "FF7575"


secY3 =
    "FF2B2B"


secY4 =
    "FF0000"



--*** Complement color:


complement0 =
    "3FCACA"


complement1 =
    "98EDED"


complement2 =
    "65DCDC"


complement3 =
    "1FB8B8"


complement4 =
    "00A4A4"


baseSpacing =
    (Css.rem 1)


baseSpacingSm =
    (Css.rem 0.5)


baseSpacingXsm =
    (Css.rem 0.25)



-- rgb(  0,164,164) = rgba(  0,164,164,1) = rgb0(0,0.643,0.643)
--
--
--#####  Generated by Paletton.com (c) 2002-2014


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
            , fontSize (pct 100)
            , margin (px 0)
            , textAlign center
            , property "user-select" "none"
            ]
        , id Page
            [ padding baseSpacing
            ]
        , class Header
            [ fontSize (Css.rem 2.6)
            , textAlign left
            , color (hex primary0)
            ]
        , class HeaderRule
            [ backgroundImage (linearGradient2 toRight (stop <| (hex complement0)) (stop <| (hex "FFFFFF")) [])
            , height (Css.rem 0.2)
            , width (pct 100)
            ]
        , class Container
            [ displayFlex
            , justifyContent flexStart
            , marginTop baseSpacing
            ]
        , class Grid
            [ displayFlex
            , flexDirection column
            ]
        , class GridRow
            [ displayFlex
            , flexDirection row
            ]
        , class GridCell
            [ width (px 50)
            , height (px 50)
            ]
        , class Sidebar
            [ padding baseSpacing
            ]
        , class ToggleOption
            [ marginBottom baseSpacing
            , displayFlex
            , children
                [ input
                    [ marginTop (Css.rem 0.2)
                    , marginRight baseSpacingSm
                    ]
                ]
            ]
        , class SliderInput
            [ marginBottom baseSpacing
            , displayFlex
            , flexDirection column
            ]
        , class SidebarHeading
            [ marginBottom baseSpacing
            , fontWeight bold
            , textAlign left
            , fontSize (Css.rem 1.2)
            ]
        , button
            [ backgroundImage none
            , backgroundColor (hex primary2)
            , display block

            --, border3 (px 2) solid (hex primary3)
            , border (Css.rem 0)
            , color (rgba 0 0 0 0.8)
            , padding baseSpacingSm
            , minWidth (Css.rem 10)
            , borderRadius baseSpacingXsm
            , marginBottom baseSpacing
            , marginRight auto
            , marginLeft auto
            , fontWeight bold
            , outline none
            , disabled
                [ backgroundColor (hex primary1)
                , color (hex "CCCCCC")
                , border3 (px 2) solid (hex primary2)
                ]
            ]
        ]
