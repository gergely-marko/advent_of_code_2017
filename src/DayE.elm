module DayE exposing (main)

import DayA exposing (knot_hash)
import Html exposing (Html)
import Array.Hamt as Array exposing (Array)
import Bitwise
import Char
import Hex


main : Html msg
main =
    let
        part_1 =
            "flqrgnkx"
                |> input_to_array
                |> count_used_squares
                |> toString

        part_2 =
            "part2"
    in
        Html.text <| part_1 ++ " / " ++ part_2


process_line : Int -> Int -> Array (Array (Maybe Int)) -> Array (Array (Maybe Int))
process_line row col acc =
    if row > 127 then
        acc
    else if col > 127 then
        process_line (row + 1) 0 acc
    else
        let
            this_row =
                acc |> Array.get row

            prew_row =
                acc |> Array.get (row - 1)

            next_row =
                acc |> Array.get (row + 1)

            top =
                prew_row |> Maybe.andThen (Array.get col)

            left =
                this_row |> Maybe.andThen (Array.get (col - 1))

            right =
                this_row |> Maybe.andThen (Array.get (col + 1))

            this =
                this_row |> Maybe.andThen (Array.get col) |> Maybe.andThen identity
        in
            case this of
                Nothing ->
                    process_line row (col + 1) acc

                Just 0 ->
                    acc

                Just x ->
                    process_line row (col + 1) (fill_adjacent_blocks row col x acc)


fill_adjacent_blocks : Int -> Int -> Int -> Array (Array (Maybe Int)) -> Array (Array (Maybe Int))
fill_adjacent_blocks row col x memory =
    let
        this_row =
            memory |> Array.get row

        prew_row =
            memory |> Array.get (row - 1)

        next_row =
            memory |> Array.get (row + 1)

        top =
            prew_row |> Maybe.andThen (Array.get col)

        left =
            this_row |> Maybe.andThen (Array.get (col - 1))

        right =
            this_row |> Maybe.andThen (Array.get (col + 1))

        new_prew_row =
            Maybe.map2
    in


count_used_squares : List String -> Int
count_used_squares list =
    list
        |> String.concat
        |> String.toList
        |> List.map
            (\c ->
                case c of
                    '0' ->
                        0

                    '1' ->
                        1

                    _ ->
                        Debug.crash "???"
            )
        |> List.sum


input_to_array : String -> List String
input_to_array input =
    List.range 0 127
        |> List.map (input_to_array_helper input)


input_to_array_helper : String -> Int -> String
input_to_array_helper input index =
    index
        |> toString
        |> (\i -> input ++ "-" ++ i)
        |> knot_hash
        |> String.toList
        |> List.map char_to_bits
        |> String.concat


char_to_bits : Char -> String
char_to_bits c =
    case c of
        '0' ->
            "0000"

        '1' ->
            "0001"

        '2' ->
            "0010"

        '3' ->
            "0011"

        '4' ->
            "0100"

        '5' ->
            "0101"

        '6' ->
            "0110"

        '7' ->
            "0111"

        '8' ->
            "1000"

        '9' ->
            "1001"

        'a' ->
            "1010"

        'b' ->
            "1011"

        'c' ->
            "1100"

        'd' ->
            "1101"

        'e' ->
            "1110"

        'f' ->
            "1111"

        _ ->
            Debug.crash "char_to_bit"
