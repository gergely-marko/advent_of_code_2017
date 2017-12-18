module DayI exposing (main)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            { registers = Dict.empty
            , instructions = Array.fromList instructions
            , instruction_index = 0
            , played = Nothing
            , last_recovered = Nothing
            }
                |> first_rcv
                |> toString

        part_2 =
            execute_programs program_0 program_1
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


type alias Model =
    { registers : Dict Char Int
    , instructions : Array Instruction
    , instruction_index : Int
    , played : Maybe Int
    , last_recovered : Maybe Int
    }


type alias Program =
    { registers : Dict Char Int
    , instructions : Array Instruction
    , instruction_index : Int
    , rcv_que : List Int
    , send_counter : Int
    }


program_0 : Program
program_0 =
    { registers = Dict.fromList [ ( 'p', 0 ) ]
    , instructions = Array.fromList instructions
    , instruction_index = 0
    , rcv_que = []
    , send_counter = 0
    }


program_1 : Program
program_1 =
    { registers = Dict.fromList [ ( 'p', 1 ) ]
    , instructions = Array.fromList instructions
    , instruction_index = 0
    , rcv_que = []
    , send_counter = 0
    }


type Instruction
    = Snd Char
    | SetR Char Char
    | SetV Char Int
    | AddR Char Char
    | AddV Char Int
    | MulR Char Char
    | MulV Char Int
    | ModR Char Char
    | ModV Char Int
    | Rcv Char
    | JgzRR Char Char
    | JgzRV Char Int
    | JgzVV Int Int
    | Halt


execute_programs : Program -> Program -> Int
execute_programs program_0 program_1 =
    let
        inst_0 =
            next_instruction program_0

        inst_1 =
            next_instruction program_1

        ( program_0_1, out_0 ) =
            step_program inst_0 program_0

        ( program_1_1, out_1 ) =
            step_program inst_1 program_1

        program_0_2 =
            process_send out_1 program_0_1

        program_1_2 =
            process_send out_0 program_1_1
    in
    if check_deadlock program_0_2 program_1_2 then
        program_1_2.send_counter
    else
        execute_programs program_0_2 program_1_2


check_deadlock : Program -> Program -> Bool
check_deadlock program_0 program_1 =
    case ( next_instruction program_0, next_instruction program_1 ) of
        ( Halt, Halt ) ->
            True

        ( Rcv _, Halt ) ->
            program_0.rcv_que == []

        ( Halt, Rcv _ ) ->
            program_1.rcv_que == []

        ( Rcv _, Rcv _ ) ->
            program_0.rcv_que == [] && program_1.rcv_que == []

        _ ->
            False


process_send : Maybe Int -> Program -> Program
process_send m_value program =
    m_value
        |> Maybe.map (\v -> { program | rcv_que = program.rcv_que ++ [ v ] })
        |> Maybe.withDefault program


next_instruction : Program -> Instruction
next_instruction program =
    program.instructions
        |> Array.get program.instruction_index
        |> Maybe.withDefault Halt


step_program : Instruction -> Program -> ( Program, Maybe Int )
step_program instruction program =
    case instruction of
        Snd key ->
            ( { program
                | send_counter = program.send_counter + 1
                , instruction_index = program.instruction_index + 1
              }
            , Dict.get key program.registers |> Maybe.withDefault 0 |> Just
            )

        SetR key key2 ->
            program.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> step_program (SetV key v) program)

        SetV key v ->
            ( { program
                | registers = Dict.insert key v program.registers
                , instruction_index = program.instruction_index + 1
              }
            , Nothing
            )

        AddR key key2 ->
            program.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> step_program (AddV key v) program)

        AddV key v2 ->
            program.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> v1 + v2)
                |> (\v ->
                        ( { program
                            | registers = Dict.insert key v program.registers
                            , instruction_index = program.instruction_index + 1
                          }
                        , Nothing
                        )
                   )

        MulR key key2 ->
            program.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> step_program (MulV key v) program)

        MulV key v2 ->
            program.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> v1 * v2)
                |> (\v ->
                        ( { program
                            | registers = Dict.insert key v program.registers
                            , instruction_index = program.instruction_index + 1
                          }
                        , Nothing
                        )
                   )

        ModR key key2 ->
            program.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> step_program (ModV key v) program)

        ModV key v2 ->
            program.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> Basics.rem v1 v2)
                |> (\v ->
                        ( { program
                            | registers = Dict.insert key v program.registers
                            , instruction_index = program.instruction_index + 1
                          }
                        , Nothing
                        )
                   )

        Rcv key ->
            case program.rcv_que of
                [] ->
                    ( program
                    , Nothing
                    )

                v :: tail ->
                    ( { program
                        | registers = Dict.insert key v program.registers
                        , rcv_que = tail
                        , instruction_index = program.instruction_index + 1
                      }
                    , Nothing
                    )

        JgzRR key key2 ->
            program.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> step_program (JgzRV key v) program)

        JgzRV key v2 ->
            program.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> step_program (JgzVV v1 v2) program)

        JgzVV v1 v2 ->
            if v1 > 0 then
                ( { program | instruction_index = program.instruction_index + v2 }
                , Nothing
                )
            else
                ( { program | instruction_index = program.instruction_index + 1 }
                , Nothing
                )

        Halt ->
            ( { program | instruction_index = -1 }
            , Nothing
            )


first_rcv : Model -> Int
first_rcv model =
    case model.last_recovered of
        Just f ->
            f

        Nothing ->
            case Array.get model.instruction_index model.instructions of
                Just i ->
                    first_rcv (execute i model)

                Nothing ->
                    Debug.crash "bad_instruction_index"


execute : Instruction -> Model -> Model
execute instruction model =
    case instruction of
        Snd key ->
            { model | played = Dict.get key model.registers }
                |> inc_instruction_index

        SetR key key2 ->
            model.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> execute (SetV key v) model)

        SetV key v ->
            { model | registers = Dict.insert key v model.registers }
                |> inc_instruction_index

        AddR key key2 ->
            model.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> execute (AddV key v) model)

        AddV key v2 ->
            model.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> v1 + v2)
                |> (\v -> { model | registers = Dict.insert key v model.registers })
                |> inc_instruction_index

        MulR key key2 ->
            model.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> execute (MulV key v) model)

        MulV key v2 ->
            model.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> v1 * v2)
                |> (\v -> { model | registers = Dict.insert key v model.registers })
                |> inc_instruction_index

        ModR key key2 ->
            model.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> execute (ModV key v) model)

        ModV key v2 ->
            model.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> Basics.rem v1 v2)
                |> (\v -> { model | registers = Dict.insert key v model.registers })
                |> inc_instruction_index

        Rcv key ->
            model.registers
                |> Dict.get key
                |> Maybe.andThen
                    (\v ->
                        if v /= 0 then
                            Just v
                        else
                            Nothing
                    )
                |> Maybe.map (\_ -> { model | last_recovered = model.played })
                |> Maybe.withDefault model
                |> inc_instruction_index

        JgzRR key key2 ->
            model.registers
                |> Dict.get key2
                |> Maybe.withDefault 0
                |> (\v -> execute (JgzRV key v) model)

        JgzRV key v2 ->
            model.registers
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\v1 -> execute (JgzVV v1 v2) model)

        JgzVV v1 v2 ->
            if v1 > 0 then
                { model | instruction_index = model.instruction_index + v2 }
            else
                inc_instruction_index model

        Halt ->
            model


inc_instruction_index : Model -> Model
inc_instruction_index model =
    { model | instruction_index = model.instruction_index + 1 }


instructions : List Instruction
instructions =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map parse_instruction


parse_instruction : String -> Instruction
parse_instruction line =
    case String.words line of
        [ "snd", x ] ->
            Snd (str_to_char x)

        [ "set", x, y ] ->
            String.toInt y
                |> Result.map (SetV (str_to_char x))
                |> Result.withDefault (SetR (str_to_char x) (str_to_char y))

        [ "add", x, y ] ->
            String.toInt y
                |> Result.map (AddV (str_to_char x))
                |> Result.withDefault (AddR (str_to_char x) (str_to_char y))

        [ "mul", x, y ] ->
            String.toInt y
                |> Result.map (MulV (str_to_char x))
                |> Result.withDefault (MulR (str_to_char x) (str_to_char y))

        [ "mod", x, y ] ->
            String.toInt y
                |> Result.map (ModV (str_to_char x))
                |> Result.withDefault (ModR (str_to_char x) (str_to_char y))

        [ "rcv", x ] ->
            Rcv (str_to_char x)

        [ "jgz", x, y ] ->
            case ( String.toInt x, String.toInt y ) of
                ( Ok v1, Ok v2 ) ->
                    JgzVV v1 v2

                ( Err e1, Ok v2 ) ->
                    JgzRV (str_to_char x) v2

                ( Err e1, Err e2 ) ->
                    JgzRR (str_to_char x) (str_to_char y)

                _ ->
                    Debug.crash "parse_jgz"

        _ ->
            Debug.crash "parse_instruction"


str_to_char : String -> Char
str_to_char str =
    case String.uncons str of
        Just ( c, _ ) ->
            c

        Nothing ->
            Debug.crash "str_to_char"


input : String
input =
    """
set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 464
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19
"""
