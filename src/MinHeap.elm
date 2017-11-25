module MinHeap exposing (empty, insert, deleteMin)

import Array
import Maybe


type alias MinHeap comparable =
    Array.Array comparable


empty : MinHeap comparable
empty =
    Array.empty


head : MinHeap comparable -> Maybe comparable
head =
    Array.get 0


bubbleUp_ : Int -> MinHeap comparable -> MinHeap comparable
bubbleUp_ i h =
    if i == 0 then
        h
    else
        let
            p =
                (parentIdx_ i)
        in
            case Maybe.map2 (<) (Array.get i h) (Array.get p h) of
                Nothing ->
                    h

                Just False ->
                    h

                Just True ->
                    bubbleUp_ p (swap_ i p h)


bubbleDown_ : Int -> MinHeap comparable -> MinHeap comparable
bubbleDown_ i h =
    if i > (Array.length h) - 1 then
        h
    else
        let
            li =
                leftIdx_ i

            ri =
                rightIdx_ i

            ix =
                Array.get i h

            lx =
                Array.get li h

            rx =
                Array.get ri h
        in
            case ( Maybe.map2 (<) ix lx, Maybe.map2 (<) ix rx, Maybe.map2 (<) lx rx ) of
                ( Just True, Just True, Just True ) ->
                    bubbleDown_ li (swap_ i li h)

                ( _, Just True, _ ) ->
                    bubbleDown_ li (swap_ i ri h)

                ( Just True, _, _ ) ->
                    bubbleDown_ li (swap_ i li h)

                _ ->
                    h


parentIdx_ : Int -> Int
parentIdx_ x =
    (x - 1) // 2


leftIdx_ : Int -> Int
leftIdx_ x =
    x * 2 + 1


rightIdx_ : Int -> Int
rightIdx_ x =
    x * 2 + 2


swap_ : Int -> Int -> MinHeap comparable -> MinHeap comparable
swap_ i j h =
    Maybe.map2 (\ix jx -> h |> Array.set i jx |> Array.set j ix) (Array.get i h) (Array.get j h)
        |> Maybe.withDefault h


insert : comparable -> MinHeap comparable -> MinHeap comparable
insert x h =
    let
        onlyX =
            Array.initialize 1 (always x)

        l =
            Array.length h
    in
        case l of
            0 ->
                onlyX

            _ ->
                Array.append h onlyX
                    |> swap_ 0 l
                    |> bubbleUp_ l


deleteMin : MinHeap comparable -> ( Maybe comparable, MinHeap comparable )
deleteMin h =
    let
        l =
            Array.length h
    in
        case l of
            0 ->
                ( Nothing, h )

            _ ->
                let
                    v =
                        Array.get 0 h

                    rem =
                        h
                            |> swap_ 0 (l - 1)
                            |> Array.slice 0 (l - 1)
                            |> bubbleDown_ 0
                in
                    ( v, rem )
