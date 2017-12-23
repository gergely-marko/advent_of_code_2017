module DayN exposing (main)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)


main : Html msg
main =
    let
        part_1 =
            parse input
                |> run
                |> .mul_counter
                |> toString

        part_2 =
            parse input2
                |> run
                |> .registers
                |> value "h"
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


type Instruction
    = SetR String String
    | SetV String Int
    | SubR String String
    | SubV String Int
    | MulR String String
    | MulV String Int
    | JnzRR String String
    | JnzRV String Int
    | JnzVV Int Int
    | ModR String String
    | ModV String Int


type alias Program =
    { registers : Dict String Int
    , instruction_index : Int
    , mul_counter : Int
    , instructions : Array Instruction
    }


run : Program -> Program
run program =
    case step program of
        Just p ->
            run p

        Nothing ->
            program


step : Program -> Maybe Program
step program =
    program.instructions
        |> Array.get program.instruction_index
        |> Maybe.map (\inst -> execute program.registers inst)
        |> Maybe.map
            (\( registers, delta_inst_index, delta_mul_counter ) ->
                { program
                    | registers = registers
                    , instruction_index = program.instruction_index + delta_inst_index
                    , mul_counter = program.mul_counter + delta_mul_counter
                }
            )


execute : Dict String Int -> Instruction -> ( Dict String Int, Int, Int )
execute registers inst =
    case inst of
        SetR r1 r2 ->
            execute registers <| SetV r1 (value r2 registers)

        SetV r v ->
            ( Dict.insert r v registers, 1, 0 )

        SubR r1 r2 ->
            execute registers <| SubV r1 (value r2 registers)

        SubV r v ->
            ( Dict.insert r (value r registers - v) registers
            , 1
            , 0
            )

        MulR r1 r2 ->
            execute registers <| MulV r1 (value r2 registers)

        MulV r v ->
            ( Dict.insert r (value r registers * v) registers
            , 1
            , 1
            )

        JnzRR r1 r2 ->
            execute registers <| JnzVV (value r1 registers) (value r2 registers)

        JnzRV r v ->
            execute registers <| JnzVV (value r registers) v

        JnzVV v1 v2 ->
            if v1 /= 0 then
                ( registers, v2, 0 )
            else
                ( registers, 1, 0 )

        ModR r1 r2 ->
            execute registers <| ModV r1 (value r2 registers)

        ModV r v ->
            ( Dict.insert r (Basics.rem (value r registers) v) registers
            , 1
            , 0
            )


value : String -> Dict String Int -> Int
value register registers =
    registers
        |> Dict.get register
        |> Maybe.withDefault 0


parse : String -> Program
parse input =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map parse_line
        |> Array.fromList
        |> Program Dict.empty 0 0


parse_line : String -> Instruction
parse_line line =
    case String.words line of
        [ inst, p1, p2 ] ->
            case ( inst, String.toInt p1, String.toInt p2 ) of
                ( "set", Err _, Err _ ) ->
                    SetR p1 p2

                ( "set", Err _, Ok val ) ->
                    SetV p1 val

                ( "mul", Err _, Err _ ) ->
                    MulR p1 p2

                ( "mul", Err _, Ok val ) ->
                    MulV p1 val

                ( "sub", Err _, Err _ ) ->
                    SubR p1 p2

                ( "sub", Err _, Ok val ) ->
                    SubV p1 val

                ( "jnz", Err _, Err _ ) ->
                    JnzRR p1 p2

                ( "jnz", Err _, Ok val ) ->
                    JnzRV p1 val

                ( "jnz", Ok v1, Ok v2 ) ->
                    JnzVV v1 v2

                ( "mod", Err _, Err _ ) ->
                    ModR p1 p2

                ( "mod", Err _, Ok val ) ->
                    ModV p1 val

                _ ->
                    Debug.crash "parse_line"

        _ ->
            Debug.crash "parse_line"


input : String
input =
    """
set b 79
set c 79
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23
"""


input2 : String
input2 =
    """
set b 79
set c 79
mul b 100
sub b -100000
set c b
sub c -17000
set d 2
set p b
mod p d
jnz p 3
sub h -1
jnz 1 5
sub d -1
set g d
sub g b
jnz g -8
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -15
"""
