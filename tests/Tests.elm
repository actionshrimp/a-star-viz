module Tests exposing (..)

import Expect
import Fuzz
import MinHeap
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "MinHeap"
        [ fuzz (Fuzz.int) "Insert then deleteMin" <|
            \i ->
                Expect.equal
                    (MinHeap.empty
                        |> MinHeap.insert i
                        |> MinHeap.deleteMin
                    )
                    ( Just i, MinHeap.empty )
        , fuzz (Fuzz.list Fuzz.int) "Insert a bunch then deleteMin" <|
            \l ->
                Expect.equal
                    (List.foldl MinHeap.insert MinHeap.empty l
                        |> MinHeap.deleteMin
                        |> Tuple.first
                    )
                    (List.head (List.sort l))
        ]
