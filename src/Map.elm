module Map exposing (..)

import Dict
import Random exposing (Generator)
import Types exposing (..)


toTiles : List (List Tile) -> Tiles
toTiles tileLists =
    let
        d =
            tileLists
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
    in
        { l = d |> Dict.toList
        , d = d
        }


emptyMap : ( Int, Int ) -> Map
emptyMap ( w, h ) =
    let
        tileLists =
            List.repeat h (List.repeat w E)
    in
        { size = ( w, h )
        , tiles = toTiles tileLists
        , start = ( 0, 0 )
        , goal = ( w - 1, h - 1 )
        , allowDiagonal = True
        }


removeEndpoints : Map -> Tiles -> Tiles
removeEndpoints { start, goal } tiles =
    let
        d =
            tiles.d
                |> Dict.update start (Maybe.map (always E))
                |> Dict.update goal (Maybe.map (always E))
    in
        { d = d
        , l = d |> Dict.toList
        }


generateMaze : List Coord
generateMaze =
    []


generateRandom : ( Int, Int ) -> Float -> Generator (List (List Tile))
generateRandom ( w, h ) probability =
    Random.list h
        (Random.list w
            (Random.map
                (\x ->
                    if x < probability then
                        Rock
                    else
                        E
                )
                (Random.float 0 1)
            )
        )
