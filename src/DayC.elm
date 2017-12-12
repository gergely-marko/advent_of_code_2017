module DayC exposing (main)

import DayCInput
import Array.Hamt as Array exposing (Array)
import Set exposing (Set)
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            group 0
                |> Set.size
                |> toString

        part_2 =
            groups
                |> List.length
                |> toString
    in
        Html.text <| part_1 ++ " / " ++ part_2


direct_neighbours : Int -> Array (List Int) -> Set Int
direct_neighbours root programs =
    programs
        |> Array.toList
        |> List.filter (List.member root)
        |> List.concat
        |> Set.fromList


collect_neighbours : Array (List Int) -> Set Int -> Set Int -> Set Int
collect_neighbours programs remaining checked =
    case Set.toList remaining of
        [] ->
            checked

        p :: tail ->
            let
                new_checked =
                    Set.insert p checked

                new_remaining =
                    programs
                        |> Array.get p
                        |> Maybe.map (\l -> List.concat [ tail, l ])
                        |> Maybe.withDefault tail
                        |> List.filter (\i -> not (Set.member i new_checked))
                        |> Set.fromList
            in
                collect_neighbours programs new_remaining new_checked


groups : List (Set Int)
groups =
    List.range 0 (Array.length DayCInput.programs - 1)
        |> List.map group
        |> (\all_groups -> filter_groups all_groups [])


group : Int -> Set Int
group program =
    direct_neighbours program DayCInput.programs
        |> (\start -> collect_neighbours DayCInput.programs start (Set.empty))


filter_groups : List (Set Int) -> List (Set Int) -> List (Set Int)
filter_groups programs acc =
    case programs of
        [] ->
            acc

        p :: tail ->
            tail
                |> List.filter (\q -> Set.intersect p q |> Set.size |> flip (>) 0)
                |> List.length
                |> (\l ->
                        if l == 0 then
                            filter_groups tail ((Debug.log "p" p) :: acc)
                        else
                            filter_groups tail acc
                   )
