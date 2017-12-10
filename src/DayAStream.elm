module DayAStream exposing (values, lengths, input2)

import Array.Hamt as Array exposing (Array)


input2 : String
input2 =
    real_lengths


real_lengths : String
real_lengths =
    "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110"


values : Array Int
values =
    real_input
        |> Array.fromList


lengths : List Int
lengths =
    real_lengths
        |> String.split ","
        |> List.filterMap (String.toInt >> Result.toMaybe)


test_lengths : String
test_lengths =
    "3,4,1,5"


test_input : List Int
test_input =
    List.range 0 4


real_input : List Int
real_input =
    List.range 0 255
