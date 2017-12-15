module DayF exposing (main)

import Html exposing (Html)
import Bitwise


main : Html msg
main =
    let
        part_1 =
            count_pairs (generate 16807) (generate 48271) 40000000 783 325 0
                |> toString

        part_2 =
            count_pairs (generate_picky 4 16807) (generate_picky 8 48271) 5000000 783 325 0
                |> toString
    in
        Html.text <| part_1 ++ " / " ++ part_2


count_pairs : (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int -> Int -> Int
count_pairs generator_a generator_b count prev_a prev_b match_count =
    case count of
        0 ->
            match_count

        _ ->
            let
                next_a =
                    generator_a prev_a

                next_b =
                    generator_b prev_b

                do_match =
                    (Bitwise.and next_a 65535) == (Bitwise.and next_b 65535)
            in
                count_pairs
                    generator_a
                    generator_b
                    (count - 1)
                    next_a
                    next_b
                    (if do_match then
                        match_count + 1
                     else
                        match_count
                    )


generate_picky : Int -> Int -> Int -> Int
generate_picky divider factor prev =
    let
        next =
            generate factor prev
    in
        if Basics.rem next divider == 0 then
            next
        else
            generate_picky divider factor next


generate : Int -> Int -> Int
generate factor prev =
    Basics.rem (prev * factor) 2147483647
