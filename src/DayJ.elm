module DayJ exposing (main)

import DayJInput as Input
import Html exposing (Html)
import Matrix exposing (Matrix)


main : Html msg
main =
    let
        ( path, step_count ) =
            solve init 0
    in
    Html.text <| path ++ " / " ++ toString step_count


type alias Model =
    { maze : Matrix Char
    , position : ( Int, Int )
    , direction : Direction
    , path : String
    }


type Direction
    = Right
    | Left
    | Up
    | Down
    | Halt


init : Model
init =
    let
        maze =
            Input.input
                |> String.lines
                |> List.filter (\l -> String.length l > 0)
                |> List.map String.toList
                |> Matrix.fromList
    in
    { maze = maze
    , position = ( 0, entry maze 0 )
    , direction = Down
    , path = ""
    }


solve : Model -> Int -> ( String, Int )
solve model step_count =
    case model.direction of
        Halt ->
            ( model.path, step_count - 1 )

        _ ->
            solve (next_step model) (step_count + 1)


next_step : Model -> Model
next_step model =
    let
        sign =
            case Matrix.get model.position model.maze of
                Just c ->
                    c

                Nothing ->
                    Debug.crash "bad_position"

        path =
            case sign of
                '|' ->
                    model.path

                '-' ->
                    model.path

                '+' ->
                    model.path

                ' ' ->
                    model.path

                c ->
                    model.path ++ String.fromChar c

        direction =
            case ( sign, model.direction ) of
                ( _, Halt ) ->
                    Halt

                ( ' ', _ ) ->
                    Halt

                ( '+', dir ) ->
                    new_direction model.maze model.position model.direction

                _ ->
                    model.direction

        position =
            case direction of
                Right ->
                    model.position |> (\( row, col ) -> ( row, col + 1 ))

                Left ->
                    model.position |> (\( row, col ) -> ( row, col - 1 ))

                Up ->
                    model.position |> (\( row, col ) -> ( row - 1, col ))

                Down ->
                    model.position |> (\( row, col ) -> ( row + 1, col ))

                Halt ->
                    model.position
    in
    { model
        | position = position
        , direction = direction
        , path = path
    }


new_direction : Matrix Char -> ( Int, Int ) -> Direction -> Direction
new_direction maze ( row, col ) direction =
    let
        right =
            Matrix.get ( row, col + 1 ) maze
                |> Maybe.andThen
                    (\c ->
                        if direction /= Left then
                            case c of
                                ' ' ->
                                    Nothing

                                _ ->
                                    Just Right
                        else
                            Nothing
                    )

        left =
            Matrix.get ( row, col - 1 ) maze
                |> Maybe.andThen
                    (\c ->
                        if direction /= Right then
                            case c of
                                ' ' ->
                                    Nothing

                                _ ->
                                    Just Left
                        else
                            Nothing
                    )

        up =
            Matrix.get ( row - 1, col ) maze
                |> Maybe.andThen
                    (\c ->
                        if direction /= Down then
                            case c of
                                ' ' ->
                                    Nothing

                                _ ->
                                    Just Up
                        else
                            Nothing
                    )

        down =
            Matrix.get ( row + 1, col ) maze
                |> Maybe.andThen
                    (\c ->
                        if direction /= Up then
                            case c of
                                ' ' ->
                                    Nothing

                                _ ->
                                    Just Down
                        else
                            Nothing
                    )
    in
    [ right, left, up, down ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault Halt


entry : Matrix Char -> Int -> Int
entry maze col =
    case Matrix.get ( 0, col ) maze of
        Just '|' ->
            col

        Just ' ' ->
            entry maze (col + 1)

        _ ->
            Debug.crash "entry_not_found"
