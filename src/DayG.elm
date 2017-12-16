module DayG exposing (main)

import Array.Hamt as Array exposing (Array)
import Char
import DayGInput
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            dance moves programs
                |> Array.toList
                |> String.fromList

        part_2 =
            let
                num =
                    number_of_runs
                        moves
                        programs
                        programs
                        0

                times =
                    Basics.rem 1000000000 num
            in
            dances times moves programs
                |> Array.toList
                |> String.fromList
    in
    Html.text <| part_1 ++ " / " ++ part_2


number_of_runs : List Move -> Array Char -> Array Char -> Int -> Int
number_of_runs moves programs new_programs num =
    if num > 0 && Array.toList programs == Array.toList new_programs then
        num
    else if num > 100 then
        num
    else
        number_of_runs
            moves
            programs
            (dance moves new_programs)
            (num + 1)


dances : Int -> List Move -> Array Char -> Array Char
dances num moves programs =
    if num == 0 then
        programs
    else
        dances
            (num - 1)
            moves
            (dance moves programs)


dance : List Move -> Array Char -> Array Char
dance moves programs =
    case moves of
        [] ->
            programs

        (Spin num) :: tail ->
            programs
                |> spin num
                |> dance tail

        (Exchange pos_1 pos_2) :: tail ->
            programs
                |> exchange pos_1 pos_2
                |> dance tail

        (Partner p1 p2) :: tail ->
            programs
                |> partner p1 p2
                |> dance tail


programs : Array Char
programs =
    List.range (Char.toCode 'a') (Char.toCode 'p')
        |> List.map Char.fromCode
        |> Array.fromList


moves : List Move
moves =
    DayGInput.input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> String.concat
        |> String.split ","
        |> List.filter (\w -> String.length w > 0)
        |> List.map word_to_move


word_to_move : String -> Move
word_to_move word =
    case String.uncons word of
        Just ( m, data ) ->
            case m of
                's' ->
                    word_to_spin data

                'x' ->
                    word_to_exchange data

                'p' ->
                    word_to_partner data

                c ->
                    let
                        err =
                            Debug.log "bad_start" c
                    in
                    Debug.crash "input"

        Nothing ->
            Debug.crash "input"


word_to_spin : String -> Move
word_to_spin word =
    case String.toInt word of
        Ok num ->
            Spin num

        Err _ ->
            Debug.crash "input"


word_to_exchange : String -> Move
word_to_exchange word =
    case String.split "/" word of
        w1 :: w2 :: [] ->
            case ( String.toInt w1, String.toInt w2 ) of
                ( Ok pos_1, Ok pos_2 ) ->
                    Exchange pos_1 pos_2

                _ ->
                    Debug.crash "input"

        _ ->
            Debug.crash "input"


word_to_partner : String -> Move
word_to_partner word =
    case String.split "/" word of
        w1 :: w2 :: [] ->
            case ( String.uncons w1, String.uncons w2 ) of
                ( Just ( p1, t1 ), Just ( p2, t2 ) ) ->
                    Partner p1 p2

                _ ->
                    Debug.crash "input"

        _ ->
            Debug.crash "input"


type Move
    = Spin Int
    | Exchange Int Int
    | Partner Char Char


spin : Int -> Array Char -> Array Char
spin num programs =
    programs
        |> Array.toList
        |> List.reverse
        |> spin_helper num
        |> List.reverse
        |> Array.fromList


spin_helper : Int -> List Char -> List Char
spin_helper num programs =
    if num == 0 then
        programs
    else
        case programs of
            x :: tail ->
                spin_helper (num - 1) (tail ++ [ x ])

            _ ->
                Debug.crash "spin_helper"


exchange : Int -> Int -> Array Char -> Array Char
exchange pos_1 pos_2 programs =
    case ( Array.get pos_1 programs, Array.get pos_2 programs ) of
        ( Just p1, Just p2 ) ->
            programs
                |> Array.set pos_1 p2
                |> Array.set pos_2 p1

        _ ->
            Debug.crash "exchange"


partner : Char -> Char -> Array Char -> Array Char
partner p1 p2 programs =
    case ( index_of_program p1 programs, index_of_program p2 programs ) of
        ( Just pos_1, Just pos_2 ) ->
            exchange pos_1 pos_2 programs

        _ ->
            Debug.crash "partner"


index_of_program : Char -> Array Char -> Maybe Int
index_of_program p programs =
    programs
        |> Array.toList
        |> List.indexedMap (,)
        |> find (\( i, pp ) -> p == pp)
        |> Maybe.map Tuple.first


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] ->
            Nothing

        a :: tail ->
            if f a then
                Just a
            else
                find f tail
