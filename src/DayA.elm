module DayA exposing (main)

import DayAStream exposing (..)
import Html exposing (Html)
import Array.Hamt as Array exposing (Array)
import Char
import Bitwise
import Hex


main : Html msg
main =
    input2
        |> String.toList
        |> List.map Char.toCode
        |> (\l -> l ++ [ 17, 31, 73, 47, 23 ])
        |> (\input -> run 64 (Debug.log "input" input) ( 0, 0, values ))
        |> Array.toList
        |> (\l -> sparse_hash (Debug.log "list" l) [])
        |> List.map (Hex.toString >> (String.padLeft 2 '0'))
        |> String.concat
        |> Html.text


dbg_1 : Int
dbg_1 =
    let
        a =
            step lengths ( 0, 0, values )
                |> (\( index, skip, data ) -> Array.toList data)
                |> Debug.log "result"
    in
        0


sparse_hash : List Int -> List Int -> List Int
sparse_hash data acc =
    case data of
        [] ->
            acc

        input ->
            let
                block =
                    List.take 16 input

                remaining =
                    List.drop 16 input

                hash =
                    xor block
            in
                sparse_hash remaining (acc ++ [ hash ])


xor : List Int -> Int
xor stream =
    case stream of
        [] ->
            0

        x :: xs ->
            List.foldl Bitwise.xor x xs


run : Int -> List Int -> ( Int, Int, Array Int ) -> Array Int
run remaining lengths ( index, skip, data ) =
    let
        actual_index =
            safe_index index data
    in
        case remaining of
            0 ->
                data

            _ ->
                run
                    (remaining - 1)
                    lengths
                    (step lengths ( actual_index, skip, data ))


step : List Int -> ( Int, Int, Array Int ) -> ( Int, Int, Array Int )
step lengths ( index, skip, data ) =
    let
        actual_index =
            safe_index index data
    in
        case lengths of
            [] ->
                ( actual_index, skip, data )

            l :: tail ->
                step
                    tail
                    ( actual_index + l + skip
                    , skip + 1
                    , (data
                        |> trim index l []
                        |> insert actual_index data
                      )
                    )


insert : Int -> Array Int -> List Int -> Array Int
insert index data stream =
    let
        actual_index =
            safe_index index data
    in
        case stream of
            [] ->
                data

            v :: tail ->
                data
                    |> Array.set actual_index v
                    |> (\d -> insert (actual_index + 1) d tail)


trim : Int -> Int -> List Int -> Array Int -> List Int
trim index length acc data =
    let
        actual_index =
            safe_index index data

        new_acc =
            data
                |> Array.get actual_index
                |> Maybe.map (\v -> v :: acc)
                |> Maybe.withDefault acc
    in
        case length of
            0 ->
                acc

            _ ->
                trim (actual_index + 1) (length - 1) new_acc data


safe_index : Int -> Array Int -> Int
safe_index index data =
    if index < Array.length data then
        index
    else
        safe_index (index - (Array.length data)) data
