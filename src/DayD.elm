module DayD exposing (main)

import Html exposing (Html)
import Dict exposing (Dict)
import Array.Hamt as Array exposing (Array)
import DayDInput


main : Html msg
main =
    let
        part_1 =
            run layers 0 0
                |> toString

        part_2 =
            run_through layers 0
                |> toString
    in
        Html.text <| part_1 ++ " / " ++ part_2


type alias Layer =
    { depth : Int
    , delta : Int
    , position : Int
    }


run_through : Dict Int Layer -> Int -> Int
run_through firewall wait =
    if run2 firewall 0 then
        wait
    else
        run_through (step_layers firewall) (wait + 1)


run : Dict Int Layer -> Int -> Int -> Int
run firewall position acc =
    if position > trip_length then
        acc
    else
        let
            severity =
                firewall
                    |> Dict.get position
                    |> Maybe.map
                        (\layer ->
                            if layer.position == 0 then
                                position * layer.depth
                            else
                                0
                        )
                    |> Maybe.withDefault 0
        in
            run (step_layers firewall) (position + 1) (acc + severity)


{-| success : True
-}
run2 : Dict Int Layer -> Int -> Bool
run2 firewall position =
    if position > trip_length then
        True
    else
        let
            success =
                firewall
                    |> Dict.get position
                    |> Maybe.map
                        (\layer ->
                            if layer.position == 0 then
                                False
                            else
                                True
                        )
                    |> Maybe.withDefault True
        in
            if success then
                run2 (step_layers firewall) (position + 1)
            else
                False


trip_length : Int
trip_length =
    layers
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 0


layers : Dict Int Layer
layers =
    DayDInput.layers
        |> Dict.toList
        |> List.map (\( i, depth ) -> ( i, init_layer depth ))
        |> Dict.fromList


layer_keys : List Int
layer_keys =
    layers
        |> Dict.keys


init_layer : Int -> Layer
init_layer depth =
    { depth = depth, delta = 1, position = 0 }


step_layers : Dict Int Layer -> Dict Int Layer
step_layers layers =
    step_layers_helper layers layer_keys


step_layers_helper : Dict Int Layer -> List Int -> Dict Int Layer
step_layers_helper layers keys =
    case keys of
        [] ->
            layers

        k :: tail ->
            case Dict.get k layers of
                Nothing ->
                    Debug.crash "bad key"

                Just layer ->
                    step_layers_helper
                        (Dict.insert k (step_layer layer) layers)
                        tail


step_layer : Layer -> Layer
step_layer layer =
    let
        new_position =
            layer.position + layer.delta

        new_delta =
            if new_position == 0 then
                1
            else if new_position == layer.depth - 1 then
                -1
            else
                layer.delta
    in
        { layer | delta = new_delta, position = new_position }
