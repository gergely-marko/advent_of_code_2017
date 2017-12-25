module DayP exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            run 12173597 { state = 'A', pos = 0, tape = Dict.empty }
                |> checksum
                |> toString

        part_2 =
            ""
    in
    Html.text <| part_1 ++ " / " ++ part_2


type alias Model =
    { state : Char
    , pos : Int
    , tape : Dict Int Bool
    }


checksum : Model -> Int
checksum model =
    model.tape
        |> Dict.values
        |> List.filter identity
        |> List.length


run : Int -> Model -> Model
run counter model =
    if counter == 0 then
        model
    else
        run (counter - 1) (step model)


step : Model -> Model
step { state, pos, tape } =
    (case ( state, get pos tape ) of
        ( 'A', False ) ->
            ( True, 1, 'B' )

        ( 'A', True ) ->
            ( False, -1, 'C' )

        ( 'B', False ) ->
            ( True, -1, 'A' )

        ( 'B', True ) ->
            ( True, 1, 'D' )

        ( 'C', False ) ->
            ( True, 1, 'A' )

        ( 'C', True ) ->
            ( False, -1, 'E' )

        ( 'D', False ) ->
            ( True, 1, 'A' )

        ( 'D', True ) ->
            ( False, 1, 'B' )

        ( 'E', False ) ->
            ( True, -1, 'F' )

        ( 'E', True ) ->
            ( True, -1, 'C' )

        ( 'F', False ) ->
            ( True, 1, 'D' )

        ( 'F', True ) ->
            ( True, 1, 'A' )

        _ ->
            Debug.crash "step"
    )
        |> (\( value, delta, next_state ) ->
                { state = next_state
                , pos = pos + delta
                , tape = Dict.insert pos value tape
                }
           )


get : Int -> Dict Int Bool -> Bool
get pos tape =
    tape
        |> Dict.get pos
        |> Maybe.withDefault False
