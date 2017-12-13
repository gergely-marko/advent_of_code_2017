module DayDInput exposing (layers)

import Dict exposing (Dict)


layers : Dict Int Int
layers =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map
            (\line ->
                case String.words line of
                    a :: b :: [] ->
                        ( a |> (String.toInt >> Result.toMaybe >> (Maybe.withDefault 0))
                        , b |> (String.toInt >> Result.toMaybe >> (Maybe.withDefault 0))
                        )

                    _ ->
                        Debug.crash "bad input"
            )
        |> Dict.fromList


test_input : String
test_input =
    """
0 3
1 2
4 4
6 4
"""


input : String
input =
    """
0 3
1 2
2 4
4 6
6 4
8 6
10 5
12 6
14 8
16 8
18 8
20 6
22 12
24 8
26 8
28 10
30 9
32 12
34 8
36 12
38 12
40 12
42 14
44 14
46 12
48 12
50 12
52 12
54 14
56 12
58 14
60 14
62 14
64 14
70 10
72 14
74 14
76 14
78 14
82 14
86 17
88 18
96 26
"""
