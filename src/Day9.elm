module Day9 exposing (..)

import Day9Stream exposing (..)
import Html exposing (Html)


main : Html msg
main =
    stream
        |> String.toList
        |> (\s -> next_group s (init_group 0))
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


next_group : List Char -> Group -> ( Group, List Char )
next_group stream (Group group) =
    case stream of
        [] ->
            ( Group group, [] )

        c :: tail ->
            if group.garbage then
                if group.skip then
                    next_group tail (Group { group | skip = False })
                else
                    case c of
                        '>' ->
                            next_group tail (Group { group | garbage = False, content = group.content ++ [ c ] })

                        '!' ->
                            next_group tail (Group { group | skip = True })

                        char ->
                            next_group
                                tail
                                (Group
                                    { group
                                        | content = group.content ++ [ char ]
                                        , garbage_size = group.garbage_size + 1
                                    }
                                )
            else
                case c of
                    '{' ->
                        let
                            ( new_group, new_tail ) =
                                next_group tail (init_group (group.level + 1))
                        in
                            (next_group new_tail (Group { group | groups = group.groups ++ [ new_group ] }))

                    '}' ->
                        ( Group group, tail )

                    ',' ->
                        next_group tail (Group group)

                    '<' ->
                        next_group tail (Group { group | garbage = True, content = group.content ++ [ c ] })

                    char ->
                        next_group tail (Group { group | content = group.content ++ [ char ] })


debug : Int
debug =
    stream
        |> String.toList
        |> (\s -> next_group s (init_group 0))
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
