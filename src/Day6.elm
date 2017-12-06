module Day6 exposing (..)

import Html exposing (..)
import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)


main : Html msg
main =
    test_input
        |> String.split " "
        |> List.filterMap (String.toInt >> Result.toMaybe)
        |> init
        |> solve
        |> toString
        |> text


type alias Model =
    { banks : Array Int
    , steps : Int
    }


init : List Int -> Model
init list =
    { banks = Array.fromList list
    , steps = 0
    }


solve : Model -> Int
solve model =
    model
        |> iterate (Dict.singleton (model.banks |> Array.toList |> toString) 0)


iterate : Dict String Int -> Model -> Int
iterate set model =
    let
        i0 =
            start_index model.banks

        v0 =
            model.banks
                |> Array.get i0
                |> Maybe.withDefault 0

        new_model =
            { model
                | banks = fill model.banks i0 v0 True
                , steps = model.steps + 1
            }

        hash =
            new_model.banks
                |> Array.toList
                |> toString
    in
        case Dict.get hash set of
            Just old_index ->
                new_model.steps - old_index

            Nothing ->
                iterate (Dict.insert hash new_model.steps set) new_model



-- set
--     |> Dict.get hash
--     |> Maybe.map (\old_index -> new_model.steps - old_index)
--     |> Maybe.withDefault (iterate (Dict.insert hash new_model.steps set) new_model)


fill : Array Int -> Int -> Int -> Bool -> Array Int
fill array index value reset =
    let
        next_idx =
            next_index array index

        next_value =
            array
                |> Array.get next_idx
                |> Maybe.withDefault 0

        new_array =
            array
                |> (\a ->
                        if reset then
                            Array.set index 0 a
                        else
                            a
                   )
                |> Array.set next_idx (next_value + 1)
    in
        if value == 0 then
            array
        else
            fill new_array next_idx (value - 1) False


start_index : Array Int -> Int
start_index array =
    let
        max_value =
            array
                |> Array.toList
                |> List.maximum
                |> Maybe.withDefault 0
    in
        array
            |> Array.toList
            |> List.indexedMap (,)
            |> find (\( i, value ) -> value == max_value)
            |> Maybe.map (\( i, value ) -> i)
            |> Maybe.withDefault 0


next_index : Array Int -> Int -> Int
next_index array idx =
    case Array.get (idx + 1) array of
        Nothing ->
            0

        Just _ ->
            idx + 1


test_input : String
test_input =
    "0 2 7 0"


input : String
input =
    "10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6"


tibi_input : String
tibi_input =
    "4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3"


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
