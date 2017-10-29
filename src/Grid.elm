module Grid exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


gridToTerrain : List (List Tile) -> Dict Coord Tile
gridToTerrain terrainGrid =
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


emptyGrid : ( Int, Int ) -> Dict Coord Tile
emptyGrid ( w, h ) =
    let
        terrainGrid =
            List.repeat h (List.repeat w E)

        cols =
            terrainGrid
                |> List.head
                |> Maybe.withDefault []
                |> List.length

        rows =
            terrainGrid
                |> List.length
    in
        gridToTerrain terrainGrid


removeEndpoints : Model -> Dict Coord Tile -> Dict Coord Tile
removeEndpoints { start, goal } terrain =
    terrain
        |> Dict.update start (Maybe.map (always E))
        |> Dict.update goal (Maybe.map (always E))
