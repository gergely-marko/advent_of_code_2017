module DayH exposing (main)

import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            insert 2018 348 0 1 [ 0 ]
                |> find_next 2017
                |> toString

        part_2 =
            neighbour 50000000 348 0 1 0 0
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


neighbour : Int -> Int -> Int -> Int -> Int -> Int -> Int
neighbour limit step_count position length pos_of_zero n_of_zero =
    if length == limit then
        n_of_zero
    else
        let
            new_pos =
                next_pos step_count position length + 1

            new_pos_of_zero =
                if new_pos <= pos_of_zero then
                    pos_of_zero
                        |> (+) 1
                        |> (\p ->
                                if p > length then
                                    0
                                else
                                    p
                           )
                else
                    pos_of_zero

            new_n_of_zero =
                if (new_pos == pos_of_zero + 1) || (new_pos == 0 && pos_of_zero == length - 1) then
                    length
                else
                    n_of_zero

            dbg =
                if Basics.rem length 1000000 == 0 then
                    Debug.log "" length
                else
                    length
        in
        neighbour limit step_count new_pos (length + 1) new_pos_of_zero new_n_of_zero


find_next : Int -> List Int -> Int
find_next value list =
    case find_next_helper value list of
        Just v ->
            v

        Nothing ->
            List.head list
                |> Maybe.withDefault -1


find_next_helper : Int -> List Int -> Maybe Int
find_next_helper value list =
    case list of
        x1 :: x2 :: tail ->
            if x1 == value then
                Just x2
            else
                find_next_helper value (x2 :: tail)

        x1 :: [] ->
            Nothing

        _ ->
            Debug.crash "not found"


insert : Int -> Int -> Int -> Int -> List Int -> List Int
insert limit step_count position length values =
    if length == limit then
        values
    else
        let
            pos =
                next_pos step_count position length + 1

            head =
                List.take pos values

            tail =
                List.drop pos values

            new_list =
                List.concat [ head, [ length ], tail ]
        in
        insert
            limit
            step_count
            pos
            (length + 1)
            new_list


next_pos : Int -> Int -> Int -> Int
next_pos step_count position length =
    let
        delta =
            Basics.rem step_count length
    in
    if position + delta < length - 1 then
        position + delta
    else
        (position + delta) - length
