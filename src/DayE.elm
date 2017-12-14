module DayE exposing (main)

import DayA exposing (knot_hash)
import Html exposing (Html)
import Array.Hamt as Array exposing (Array)
import Bitwise
import Char
import Hex
import Matrix exposing (Matrix)


main : Html msg
main =
    let
        part_1 =
            "flqrgnkx"

        -- |> input_to_array
        -- |> count_used_squares
        -- |> toString
        part_2 =
            "flqrgnkx"
                |> input_to_array
                |> strings_to_matrix
                |> process_cell 0 0 1
                |> max_value
                |> toString
    in
        Html.text <| part_1 ++ " / " ++ part_2


process_cell : Int -> Int -> Int -> Matrix (Maybe Int) -> Matrix (Maybe Int)
process_cell row col block_id memory =
    if row > 127 then
        memory
    else if col > 127 then
        process_cell (row + 1) 0 block_id memory
    else
        case Matrix.get ( row, col ) memory |> Maybe.andThen identity of
            Just 0 ->
                let
                    top =
                        memory
                            |> Matrix.get ( row - 1, col )
                            |> Maybe.andThen identity
                            |> Maybe.andThen
                                (\x ->
                                    if x == 0 then
                                        Nothing
                                    else
                                        Just x
                                )

                    left =
                        memory
                            |> Matrix.get ( row, col - 1 )
                            |> Maybe.andThen identity
                            |> Maybe.andThen
                                (\x ->
                                    if x == 0 then
                                        Nothing
                                    else
                                        Just x
                                )

                    right =
                        memory
                            |> Matrix.get ( row, col + 1 )
                            |> Maybe.andThen identity
                            |> Maybe.andThen
                                (\x ->
                                    if x == 0 then
                                        Nothing
                                    else
                                        Just x
                                )

                    bottom =
                        memory
                            |> Matrix.get ( row + 1, col )
                            |> Maybe.andThen identity
                            |> Maybe.andThen
                                (\x ->
                                    if x == 0 then
                                        Nothing
                                    else
                                        Just x
                                )
                in
                    case top of
                        Nothing ->
                            case left of
                                Nothing ->
                                    case right of
                                        Nothing ->
                                            case bottom of
                                                Nothing ->
                                                    process_cell row (col + 1) (block_id + 1) (fill_adjacent_blocks row col block_id memory)

                                                Just x ->
                                                    process_cell row (col + 1) block_id (fill_adjacent_blocks row col x memory)

                                        Just x ->
                                            process_cell row (col + 1) block_id (fill_adjacent_blocks row col x memory)

                                Just x ->
                                    process_cell row (col + 1) block_id (fill_adjacent_blocks row col x memory)

                        Just x ->
                            process_cell row (col + 1) block_id (fill_adjacent_blocks row col x memory)

            Just x ->
                process_cell row (col + 1) block_id (fill_adjacent_blocks row col x memory)

            Nothing ->
                process_cell row (col + 1) block_id memory


fill_adjacent_blocks : Int -> Int -> Int -> Matrix (Maybe Int) -> Matrix (Maybe Int)
fill_adjacent_blocks row col x memory =
    memory
        |> Matrix.set ( row, col ) (Just x)
        |> (\memory ->
                memory
                    |> Matrix.get ( row - 1, col )
                    |> Maybe.andThen identity
                    |> Maybe.map (\_ -> Matrix.set ( row - 1, col ) (Just x) memory)
                    |> Maybe.withDefault memory
           )
        |> (\memory ->
                memory
                    |> Matrix.get ( row, col - 1 )
                    |> Maybe.andThen identity
                    |> Maybe.map (\_ -> Matrix.set ( row, col - 1 ) (Just x) memory)
                    |> Maybe.withDefault memory
           )
        |> (\memory ->
                memory
                    |> Matrix.get ( row, col + 1 )
                    |> Maybe.andThen identity
                    |> Maybe.map (\_ -> Matrix.set ( row, col + 1 ) (Just x) memory)
                    |> Maybe.withDefault memory
           )
        |> (\memory ->
                memory
                    |> Matrix.get ( row + 1, col )
                    |> Maybe.andThen identity
                    |> Maybe.map (\_ -> Matrix.set ( row + 1, col ) (Just x) memory)
                    |> Maybe.withDefault memory
           )


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


strings_to_matrix : List String -> Matrix (Maybe Int)
strings_to_matrix input =
    input
        |> List.map
            (\line ->
                line
                    |> String.toList
                    |> List.map
                        (\c ->
                            case c of
                                '0' ->
                                    Nothing

                                '1' ->
                                    Just 0

                                _ ->
                                    Debug.crash "strings_to_array"
                        )
            )
        |> Matrix.fromList


max_value : Matrix (Maybe Int) -> Int
max_value matrix =
    matrix
        |> Matrix.flatten
        |> List.filterMap identity
        |> List.maximum
        |> Maybe.withDefault 0


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
