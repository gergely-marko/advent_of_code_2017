module DayO exposing (main)

import Html exposing (Html)


main : Html msg
main =
    let
        bridges =
            build <| parse input

        max_length =
            bridges
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 0

        part_1 =
            bridges
                |> List.map strength
                |> List.maximum
                |> toString

        part_2 =
            bridges
                |> List.filter (\b -> List.length b == max_length)
                |> List.map strength
                |> List.maximum
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


build : List ( Int, Int ) -> List (List ( Int, Int ))
build ports =
    let
        bridges =
            matching_ports 0 ports
                |> List.foldl (\p acc -> [ p ] :: acc) []
                |> List.map (\b -> ( False, b ))
    in
    build_helper ports bridges


build_helper : List ( Int, Int ) -> List ( Bool, List ( Int, Int ) ) -> List (List ( Int, Int ))
build_helper ports bridges =
    let
        bridges_and_ports =
            bridges
                |> List.map
                    (\( is_finished, b ) ->
                        if is_finished then
                            ( ( True, b ), [] )
                        else
                            ports
                                |> filter_ports b
                                |> matching_ports (end_of_bridge b)
                                |> (\ps ->
                                        ( ( ps == [], b ), ps )
                                   )
                    )

        exit =
            bridges_and_ports
                |> List.all (\( ( is_finished, b ), ps ) -> is_finished)

        dbg =
            bridges
                |> List.filter (Tuple.first >> not)
                |> List.head
                |> Debug.log "bridge"
    in
    if exit then
        bridges
            |> List.map Tuple.second
    else
        bridges_and_ports
            |> List.map
                (\( ( is_finished, b ), ps ) ->
                    let
                        new_ports =
                            matching_ports (end_of_bridge b) ps
                    in
                    if new_ports == [] then
                        [ ( True, b ) ]
                    else
                        new_ports
                            |> List.map (\p -> ( False, b ++ [ p ] ))
                )
            |> List.concat
            |> build_helper ports


end_of_bridge : List ( Int, Int ) -> Int
end_of_bridge ports =
    case ports of
        [] ->
            Debug.crash "end_of_bridge"

        ( p1, p2 ) :: [] ->
            p2

        p :: tail ->
            end_of_bridge tail


serialize : List ( Int, Int ) -> List Int
serialize bridge =
    bridge
        |> List.map (\( p1, p2 ) -> [ p1, p2 ])
        |> List.concat


strength : List ( Int, Int ) -> Int
strength bridge =
    bridge
        |> serialize
        |> List.sum


filter_ports : List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
filter_ports bridge ports =
    ports
        |> List.filter
            (\( p1, p2 ) ->
                bridge
                    |> List.any (\( b1, b2 ) -> (p1 == b1 && p2 == b2) || (p1 == b2 && p2 == b1))
                    |> not
            )


matching_ports : Int -> List ( Int, Int ) -> List ( Int, Int )
matching_ports p0 ports =
    ports
        |> List.filterMap
            (\( p1, p2 ) ->
                if p1 == p0 then
                    Just ( p1, p2 )
                else if p2 == p0 then
                    Just ( p2, p1 )
                else
                    Nothing
            )


parse : String -> List ( Int, Int )
parse input =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map parse_line


parse_line : String -> ( Int, Int )
parse_line line =
    case String.split "/" line of
        [ p1, p2 ] ->
            case ( String.toInt p1, String.toInt p2 ) of
                ( Ok v1, Ok v2 ) ->
                    ( v1, v2 )

                _ ->
                    Debug.crash "parse_line"

        _ ->
            Debug.crash "parse_line"


input : String
input =
    """
42/37
28/28
29/25
45/8
35/23
49/20
44/4
15/33
14/19
31/44
39/14
25/17
34/34
38/42
8/42
15/28
0/7
49/12
18/36
45/45
28/7
30/43
23/41
0/35
18/9
3/31
20/31
10/40
0/22
1/23
20/47
38/36
15/8
34/32
30/30
30/44
19/28
46/15
34/50
40/20
27/39
3/14
43/45
50/42
1/33
6/39
46/44
22/35
15/20
43/31
23/23
19/27
47/15
43/43
25/36
26/38
1/10
"""
