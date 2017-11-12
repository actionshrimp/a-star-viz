module Map exposing (..)

import Dict
import Random exposing (Generator)
import Types exposing (..)


toTiles : List (List Tile) -> Tiles
toTiles tileLists =
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
        , allowDiagonal = False
        }


removeEndpoints : Map -> Tiles -> Tiles
removeEndpoints { start, goal } terrain =
    terrain
        |> Dict.update start (Maybe.map (always E))
        |> Dict.update goal (Maybe.map (always E))


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
