module DayM exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            parse input
                |> State ( 12, 12 ) Up 0 0
                |> step_while 10000 step1
                |> .success_counter
                |> toString

        part_2 =
            parse input
                |> State ( 12, 12 ) Up 0 0
                |> step_while 10000000 step2
                |> .success_counter
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


type Node
    = Clean
    | Weakened
    | Infected
    | Flagged


type Direction
    = Up
    | Down
    | Left
    | Right


type alias State =
    { pos : ( Int, Int )
    , dir : Direction
    , burst_count : Int
    , success_counter : Int
    , nodes : Dict ( Int, Int ) Node
    }


step_while : Int -> (State -> State) -> State -> State
step_while counter step state =
    if counter == 0 then
        state
    else
        step_while (counter - 1) step (step state)


step1 : State -> State
step1 state =
    let
        old_node =
            state.nodes
                |> Dict.get state.pos
                |> Maybe.withDefault Clean

        ( new_node, new_dir ) =
            case ( old_node, state.dir ) of
                ( Clean, Up ) ->
                    ( Infected, Left )

                ( Clean, Down ) ->
                    ( Infected, Right )

                ( Clean, Left ) ->
                    ( Infected, Down )

                ( Clean, Right ) ->
                    ( Infected, Up )

                ( Infected, Up ) ->
                    ( Clean, Right )

                ( Infected, Down ) ->
                    ( Clean, Left )

                ( Infected, Left ) ->
                    ( Clean, Up )

                ( Infected, Right ) ->
                    ( Clean, Down )

                _ ->
                    Debug.crash "step"

        ( x, y ) =
            state.pos

        new_pos =
            case new_dir of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Right ->
                    ( x + 1, y )

                Left ->
                    ( x - 1, y )
    in
    { state
        | pos = new_pos
        , dir = new_dir
        , burst_count = state.burst_count + 1
        , nodes =
            state.nodes
                |> Dict.insert state.pos new_node
        , success_counter =
            case new_node of
                Infected ->
                    state.success_counter + 1

                _ ->
                    state.success_counter
    }



-- Clean    -> Weakened -> Left
-- Weakened -> Infected -> Continue
-- Infected -> Flagged  -> Right
-- Flagged  -> Clean    -> Reverse


step2 : State -> State
step2 state =
    let
        old_node =
            state.nodes
                |> Dict.get state.pos
                |> Maybe.withDefault Clean

        ( new_node, new_dir ) =
            case ( old_node, state.dir ) of
                ( Clean, Up ) ->
                    ( Weakened, Left )

                ( Clean, Down ) ->
                    ( Weakened, Right )

                ( Clean, Left ) ->
                    ( Weakened, Down )

                ( Clean, Right ) ->
                    ( Weakened, Up )

                ( Weakened, _ ) ->
                    ( Infected, state.dir )

                ( Infected, Up ) ->
                    ( Flagged, Right )

                ( Infected, Down ) ->
                    ( Flagged, Left )

                ( Infected, Left ) ->
                    ( Flagged, Up )

                ( Infected, Right ) ->
                    ( Flagged, Down )

                ( Flagged, Up ) ->
                    ( Clean, Down )

                ( Flagged, Down ) ->
                    ( Clean, Up )

                ( Flagged, Left ) ->
                    ( Clean, Right )

                ( Flagged, Right ) ->
                    ( Clean, Left )

        ( x, y ) =
            state.pos

        new_pos =
            case new_dir of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Right ->
                    ( x + 1, y )

                Left ->
                    ( x - 1, y )
    in
    { state
        | pos = new_pos
        , dir = new_dir
        , burst_count = state.burst_count + 1
        , nodes =
            state.nodes
                |> Dict.insert state.pos new_node
        , success_counter =
            case new_node of
                Infected ->
                    state.success_counter + 1

                _ ->
                    state.success_counter
    }


parse : String -> Dict ( Int, Int ) Node
parse input =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.indexedMap parse_line
        |> List.concat
        |> Dict.fromList


parse_line : Int -> String -> List ( ( Int, Int ), Node )
parse_line y line =
    line
        |> String.toList
        |> List.map
            (\c ->
                if c == '#' then
                    Infected
                else
                    Clean
            )
        |> List.indexedMap
            (\x node ->
                ( ( x, y ), node )
            )


test_input : String
test_input =
    """
..#
#..
...
"""


input : String
input =
    """
..##.##.######...#.######
##...#...###....##.#.#.##
###.#.#.#..#.##.####.#.#.
..##.##...#..#.##.....##.
##.##...#.....#.#..#.####
.###...#.........###.####
#..##....###...#######..#
###..#.####.###.#.#......
.#....##..##...###..###.#
###.#..#.##.###.#..###...
####.#..##.#.#.#.#.#...##
##.#####.#......#.#.#.#.#
..##..####...#..#.#.####.
.####.####.####...##.#.##
#####....#...#.####.#..#.
.#..###..........#..#.#..
.#.##.#.#.##.##.#..#.#...
..##...#..#.....##.####..
..#.#...######..##..##.#.
.####.###....##...####.#.
.#####..#####....####.#..
###..#..##.#......##.###.
.########...#.#...###....
...##.#.##.#####.###.####
.....##.#.#....#..#....#.
"""
