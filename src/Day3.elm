module Day3 exposing (..)

import Html exposing (..)
import Dict exposing (Dict)


main : Html msg
main =
    div
        []
        [ div [] [ text result_1 ]
        , div [] [ text result_2 ]
        ]


result_1 : String
result_1 =
    312051
        |> fill
        |> (\model -> model.position)
        |> (\( x, y ) -> (abs x) + (abs y))
        |> toString


result_2 : String
result_2 =
    312051
        |> fill_2
        |> (\model -> model.sum)
        |> toString


type alias Position =
    ( Int, Int )


type Direction
    = Right
    | Up
    | Left
    | Down


type alias Model =
    { mem_indices : Dict Position Int
    , mem_sums : Dict Position Int
    , position : Position
    , index : Int
    , sum : Int
    , next_move : Direction
    }


fill : Int -> Model
fill max =
    iterate max init


fill_2 : Int -> Model
fill_2 max =
    iterate_2 max init


iterate : Int -> Model -> Model
iterate max_index model =
    if model.index == max_index then
        model
    else
        iterate max_index (next model)


iterate_2 : Int -> Model -> Model
iterate_2 max_sum model =
    if model.sum > max_sum then
        model
    else
        iterate_2 max_sum (next model)


init : Model
init =
    { mem_indices = Dict.insert ( 0, 0 ) 1 Dict.empty
    , mem_sums = Dict.insert ( 0, 0 ) 1 Dict.empty
    , position = ( 0, 0 )
    , index = 1
    , sum = 1
    , next_move = Right
    }


delta : Direction -> ( Int, Int )
delta move =
    case move of
        Right ->
            ( 1, 0 )

        Up ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Down ->
            ( 0, -1 )


step : Direction -> Position -> Position
step direction ( x, y ) =
    let
        ( dx, dy ) =
            delta direction
    in
        ( x + dx, y + dy )


next : Model -> Model
next model =
    let
        new_index =
            model.index + 1

        new_sum =
            sum_of_square model.mem_sums new_position

        new_position =
            step model.next_move model.position

        new_mem_indices =
            Dict.insert new_position new_index model.mem_indices

        new_mem_sums =
            Dict.insert new_position new_sum model.mem_sums

        next_move =
            case model.next_move of
                Right ->
                    if can_move new_mem_indices new_position Up then
                        Up
                    else
                        Right

                Up ->
                    if can_move new_mem_indices new_position Left then
                        Left
                    else
                        Up

                Left ->
                    if can_move new_mem_indices new_position Down then
                        Down
                    else
                        Left

                Down ->
                    if can_move new_mem_indices new_position Right then
                        Right
                    else
                        Down
    in
        { model
            | mem_indices = new_mem_indices
            , mem_sums = new_mem_sums
            , position = new_position
            , index = new_index
            , sum = new_sum
            , next_move = next_move
        }


can_move : Dict Position Int -> Position -> Direction -> Bool
can_move mem_by_pos pos dir =
    let
        next_pos =
            step dir pos
    in
        case Dict.get next_pos mem_by_pos of
            Just p ->
                False

            Nothing ->
                True


sum_of_square : Dict Position Int -> Position -> Int
sum_of_square dict ( x, y ) =
    List.sum
        [ value dict ( x + 1, y + 0 )
        , value dict ( x + 1, y + 1 )
        , value dict ( x + 0, y + 1 )
        , value dict ( x - 1, y + 1 )
        , value dict ( x - 1, y + 0 )
        , value dict ( x - 1, y - 1 )
        , value dict ( x + 0, y - 1 )
        , value dict ( x + 1, y - 1 )
        ]


value : Dict Position Int -> Position -> Int
value dict pos =
    dict
        |> Dict.get pos
        |> Maybe.withDefault 0
