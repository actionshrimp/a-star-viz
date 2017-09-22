module Styles exposing (..)

import Color
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
    | Sidebar
    | Header
    | HeaderRule


type CssIds
    = Page


--#####  Color Palette by Paletton.com
--#####  Palette URL: http://paletton.com/#uid=60u0u0km4KpbqVchjPSqBE-zJzx
--
--

grey = "333333"

--*** Primary color:
--
primary0 = "FF9F4F" -- rgb(255,159, 79) = rgba(255,159, 79,1) = rgb0(1,0.624,0.31)
primary1 = "FFCDA4" -- rgb(255,205,164) = rgba(255,205,164,1) = rgb0(1,0.804,0.643)
primary2 = "FFB475" -- rgb(255,180,117) = rgba(255,180,117,1) = rgb0(1,0.706,0.459)
primary3 = "FF8B2B" -- rgb(255,139, 43) = rgba(255,139, 43,1) = rgb0(1,0.545,0.169)
primary4 = "FF7400" -- rgb(255,116,  0) = rgba(255,116,  0,1) = rgb0(1,0.455,0)
--
--*** Secondary color (1):
--
secX0 = "FFC44F" -- rgb(255,196, 79) = rgba(255,196, 79,1) = rgb0(1,0.769,0.31)
secX1 = "FFE1A4" -- rgb(255,225,164) = rgba(255,225,164,1) = rgb0(1,0.882,0.643)
secX2 = "FFD175" -- rgb(255,209,117) = rgba(255,209,117,1) = rgb0(1,0.82,0.459)
secX3 = "FFB82B" -- rgb(255,184, 43) = rgba(255,184, 43,1) = rgb0(1,0.722,0.169)
secX4 = "FFAA00" -- rgb(255,170,  0) = rgba(255,170,  0,1) = rgb0(1,0.667,0)
--
--*** Secondary color (2):
--
secY0 = "FF4F4F" -- rgb(255, 79, 79) = rgba(255, 79, 79,1) = rgb0(1,0.31,0.31)
secY1 = "FFA4A4" -- rgb(255,164,164) = rgba(255,164,164,1) = rgb0(1,0.643,0.643)
secY2 = "FF7575" -- rgb(255,117,117) = rgba(255,117,117,1) = rgb0(1,0.459,0.459)
secY3 = "FF2B2B" -- rgb(255, 43, 43) = rgba(255, 43, 43,1) = rgb0(1,0.169,0.169)
secY4 = "FF0000" -- rgb(255,  0,  0) = rgba(255,  0,  0,1) = rgb0(1,0,0)
--
--*** Complement color:
--
complement0 = "3FCACA" -- rgb( 63,202,202) = rgba( 63,202,202,1) = rgb0(0.247,0.792,0.792)
complement1 = "98EDED" -- rgb(152,237,237) = rgba(152,237,237,1) = rgb0(0.596,0.929,0.929)
complement2 = "65DCDC" -- rgb(101,220,220) = rgba(101,220,220,1) = rgb0(0.396,0.863,0.863)
complement3 = "1FB8B8" -- rgb( 31,184,184) = rgba( 31,184,184,1) = rgb0(0.122,0.722,0.722)
complement4 = "00A4A4" -- rgb(  0,164,164) = rgba(  0,164,164,1) = rgb0(0,0.643,0.643)
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
            , fontSize (px 30)
            , margin (px 0)
            , textAlign center
            , color (hex "293c4b")
            , property "user-select" "none"
            ]
        , id Page
            [ padding (em 1)
            ]
        , class Header
            [ fontSize (em 1.2)
            , textAlign left
            , color (hex primary0)
            ]
        , class HeaderRule
            [ backgroundImage (linearGradient2 toRight (stop <| (hex complement0)) (stop <| (hex "FFFFFF")) [])
            , height (px 3)
            , width (pct 100)
            ]
        , class Container
            [ displayFlex
            , justifyContent flexStart
            , marginTop (em 1)
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
            [ padding (px 20)
            ]
        ]

