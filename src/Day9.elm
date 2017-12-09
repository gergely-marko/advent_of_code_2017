module Day9 exposing (..)

import Day9Stream exposing (..)
import Html exposing (Html)


main : Html msg
main =
    stream
        |> String.toList
        |> next_group (init_group 0)
        |> (\( group, tail ) -> render_group group)


render_group : Group -> Html msg
render_group (Group group) =
    Html.div
        []
        [ Html.div
            []
            [ Html.text (String.fromList group.content ++ " | " ++ (toString group.level)) ]
        , Html.ul
            []
            (group.groups
                |> List.map
                    (\child ->
                        Html.li
                            []
                            [ render_group child ]
                    )
            )
        ]


type Group
    = Group GroupData


type alias GroupData =
    { garbage : Bool
    , skip : Bool
    , content : List Char
    , groups : List Group
    , level : Int
    , garbage_size : Int
    }


init_group : Int -> Group
init_group level =
    Group (GroupData False False [] [] level 0)


next_group : Group -> List Char -> ( Group, List Char )
next_group (Group group) stream =
    case stream of
        [] ->
            ( Group group, [] )

        c :: tail ->
            if group.garbage then
                if group.skip then
                    next_group (Group { group | skip = False }) tail
                else
                    case c of
                        '>' ->
                            next_group (Group { group | garbage = False, content = group.content ++ [ c ] }) tail

                        '!' ->
                            next_group (Group { group | skip = True }) tail

                        char ->
                            next_group
                                (Group
                                    { group
                                        | content = group.content ++ [ char ]
                                        , garbage_size = group.garbage_size + 1
                                    }
                                )
                                tail
            else
                case c of
                    '{' ->
                        let
                            ( new_group, new_tail ) =
                                next_group (init_group (group.level + 1)) tail
                        in
                            (next_group (Group { group | groups = group.groups ++ [ new_group ] }) new_tail)

                    '}' ->
                        ( Group group, tail )

                    ',' ->
                        next_group (Group group) tail

                    '<' ->
                        next_group (Group { group | garbage = True, content = group.content ++ [ c ] }) tail

                    char ->
                        next_group (Group { group | content = group.content ++ [ char ] }) tail


debug : Int
debug =
    stream
        |> String.toList
        |> next_group (init_group 0)
        |> (\( group, tail ) ->
                let
                    dbg_levels =
                        sum_levels group |> (Debug.log "levels")

                    dbg_garbage =
                        sum_garbage group |> (Debug.log "garbage")
                in
                    0
           )


sum_levels : Group -> Int
sum_levels (Group group) =
    group.groups
        |> List.map sum_levels
        |> List.sum
        |> (+) group.level


sum_garbage : Group -> Int
sum_garbage (Group group) =
    group.groups
        |> List.map sum_garbage
        |> List.sum
        |> (+) group.garbage_size


test_stream : String
test_stream =
    "{{<!!>},{<!!>},{<!!>},{<!!>}}"
