module DayL exposing (..)

import DayLInput as Input
import Html exposing (Html)
import Matrix exposing (Matrix)


main : Html msg
main =
    let
        patterns =
            parse Input.input

        {-
           .#.
           ..#
           ###
        -}
        image =
            Matrix.fromList [ [ Size3 ( ( '.', '#', '.' ), ( '.', '.', '#' ), ( '#', '#', '#' ) ) ] ]

        part_1 =
            step_while 5 patterns image
                |> pixels_on
                |> toString

        part_2 =
            step_while 18 patterns image
                |> pixels_on
                |> toString
    in
    Html.text <| part_1 ++ " / " ++ part_2


type Square
    = Size2 M2
    | Size3 M3
    | Size4 M4


type alias M2 =
    ( ( Char, Char ), ( Char, Char ) )


type alias M3 =
    ( ( Char, Char, Char ), ( Char, Char, Char ), ( Char, Char, Char ) )


type alias M4 =
    ( ( Char, Char, Char, Char ), ( Char, Char, Char, Char ), ( Char, Char, Char, Char ), ( Char, Char, Char, Char ) )


type alias Pattern =
    { from : Square
    , to : Square
    }


pixels_on : Matrix Square -> Int
pixels_on image =
    image
        |> Matrix.flatten
        |> List.map flat
        |> List.concat
        |> List.filter (\c -> c == '#')
        |> List.length


step_while : Int -> List Pattern -> Matrix Square -> Matrix Square
step_while counter patterns image =
    let
        square_size =
            case get ( 0, 0 ) image of
                Size2 _ ->
                    "2x2"

                Size3 _ ->
                    "3x3"

                Size4 _ ->
                    "4x4"

        dbg =
            Debug.log (toString counter) <| square_size ++ " x " ++ (toString <| Matrix.colCount image) ++ " " ++ (toString <| pixels_on image)
    in
    if counter == 0 then
        image
    else
        step_while (counter - 1) patterns (step patterns image)


step : List Pattern -> Matrix Square -> Matrix Square
step patterns image =
    let
        ( filtered_patterns, new_image ) =
            case get ( 0, 0 ) image of
                Size2 _ ->
                    ( patterns
                        |> List.filter
                            (\p ->
                                case p.from of
                                    Size2 _ ->
                                        True

                                    _ ->
                                        False
                            )
                    , image
                    )

                Size3 _ ->
                    let
                        width =
                            Matrix.colCount image * 3
                    in
                    if width % 2 == 0 then
                        ( patterns
                            |> List.filter
                                (\p ->
                                    case p.from of
                                        Size2 _ ->
                                            True

                                        _ ->
                                            False
                                )
                        , explode3 image
                        )
                    else
                        ( patterns
                            |> List.filter
                                (\p ->
                                    case p.from of
                                        Size3 _ ->
                                            True

                                        _ ->
                                            False
                                )
                        , image
                        )

                Size4 _ ->
                    ( patterns
                        |> List.filter
                            (\p ->
                                case p.from of
                                    Size2 _ ->
                                        True

                                    _ ->
                                        False
                            )
                    , explode image
                    )
    in
    step_helper filtered_patterns new_image ( 0, 0 ) new_image


step_helper : List Pattern -> Matrix Square -> ( Int, Int ) -> Matrix Square -> Matrix Square
step_helper patterns source (( row, col ) as location) image =
    let
        size =
            Matrix.colCount image
    in
    if row == size then
        image
    else if col == size then
        step_helper patterns source ( row + 1, 0 ) image
    else
        step_helper patterns source ( row, col + 1 ) (replace patterns location image)


explode3 : Matrix Square -> Matrix Square
explode3 image =
    let
        size =
            Matrix.colCount image

        new_size =
            (size * 3) // 2

        new_image =
            Matrix.square new_size (\_ -> Size2 ( ( '.', '.' ), ( '.', '.' ) ))
    in
    explode3_helper image ( 0, 0 ) new_image


explode3_helper : Matrix Square -> ( Int, Int ) -> Matrix Square -> Matrix Square
explode3_helper source (( row, col ) as location) image =
    let
        size =
            Matrix.colCount source
    in
    if row >= size then
        image
    else if col >= size then
        explode3_helper source ( row + 2, 0 ) image
    else
        explode3_helper source ( row, col + 2 ) (fill3 source location image)



{-
   a00 b00 c00 | a01 b01 c01      a00 b00 | c00 a01 | b01 c01
   d00 e00 f00 | d01 e01 f01      d00 e00 | f00 d01 | e01 f01
   g00 h00 i00 | g01 h01 i01      --------+ --------+ -------
   ------------+------------      g00 h00 | i00 g01 | h01 i01
   a10 b10 c10 | a11 b11 c11      a10 b10 | c10 a11 | b11 c11
   d10 e10 f10 | d11 e11 f11      --------+ --------+ -------
   g10 h10 i10 | g11 h11 i11      d10 e10 | f10 d11 | e11 f11
                                  g10 h10 | i10 g11 | h11 i11
-}


fill3 : Matrix Square -> ( Int, Int ) -> Matrix Square -> Matrix Square
fill3 source (( row, col ) as location) image =
    let
        row0 =
            (row * 3) // 2

        row1 =
            row0 + 1

        row2 =
            row0 + 2

        col0 =
            (col * 3) // 2

        col1 =
            col0 + 1

        col2 =
            col0 + 2
    in
    case ( get ( row, col ) source, get ( row, col + 1 ) source, get ( row + 1, col ) source, get ( row + 1, col + 1 ) source ) of
        ( Size3 ( ( a00, b00, c00 ), ( d00, e00, f00 ), ( g00, h00, i00 ) ), Size3 ( ( a01, b01, c01 ), ( d01, e01, f01 ), ( g01, h01, i01 ) ), Size3 ( ( a10, b10, c10 ), ( d10, e10, f10 ), ( g10, h10, i10 ) ), Size3 ( ( a11, b11, c11 ), ( d11, e11, f11 ), ( g11, h11, i11 ) ) ) ->
            image
                |> Matrix.set ( row0, col0 ) (Size2 ( ( a00, b00 ), ( d00, e00 ) ))
                |> Matrix.set ( row0, col1 ) (Size2 ( ( c00, a01 ), ( f00, d01 ) ))
                |> Matrix.set ( row0, col2 ) (Size2 ( ( b01, c01 ), ( e01, f01 ) ))
                |> Matrix.set ( row1, col0 ) (Size2 ( ( g00, h00 ), ( a10, b10 ) ))
                |> Matrix.set ( row1, col1 ) (Size2 ( ( i00, g01 ), ( c10, a11 ) ))
                |> Matrix.set ( row1, col2 ) (Size2 ( ( h01, i01 ), ( b11, c11 ) ))
                |> Matrix.set ( row2, col0 ) (Size2 ( ( d10, e10 ), ( g10, h10 ) ))
                |> Matrix.set ( row2, col1 ) (Size2 ( ( f10, d11 ), ( i10, g11 ) ))
                |> Matrix.set ( row2, col2 ) (Size2 ( ( e11, f11 ), ( h11, i11 ) ))

        _ ->
            Debug.crash "this image can not be exploded"


explode : Matrix Square -> Matrix Square
explode image =
    let
        size =
            Matrix.colCount image

        new_image =
            Matrix.square (2 * size) (\_ -> Size2 ( ( '.', '.' ), ( '.', '.' ) ))
    in
    explode_helper image ( 0, 0 ) new_image


explode_helper : Matrix Square -> ( Int, Int ) -> Matrix Square -> Matrix Square
explode_helper source (( row, col ) as location) image =
    let
        size =
            Matrix.colCount source
    in
    if row == size then
        image
    else if col == size then
        explode_helper source ( row + 1, 0 ) image
    else
        explode_helper source ( row, col + 1 ) (fill source location image)



{-
   a b | c d
   e f | g h
   ----+----
   i j | k l
   m n | o p
-}


fill : Matrix Square -> ( Int, Int ) -> Matrix Square -> Matrix Square
fill source (( row, col ) as location) image =
    let
        row0 =
            row * 2

        row1 =
            row0 + 1

        col0 =
            col * 2

        col1 =
            col0 + 1
    in
    case get location source of
        Size4 ( ( a, b, c, d ), ( e, f, g, h ), ( i, j, k, l ), ( m, n, o, p ) ) ->
            image
                |> Matrix.set ( row0, col0 ) (Size2 ( ( a, b ), ( e, f ) ))
                |> Matrix.set ( row0, col1 ) (Size2 ( ( c, d ), ( g, h ) ))
                |> Matrix.set ( row1, col0 ) (Size2 ( ( i, j ), ( m, n ) ))
                |> Matrix.set ( row1, col1 ) (Size2 ( ( k, l ), ( o, p ) ))

        _ ->
            Debug.crash "this image can not be exploded"


replace : List Pattern -> ( Int, Int ) -> Matrix Square -> Matrix Square
replace patterns location image =
    Matrix.set location (find_pattern patterns (get location image)) image


find_pattern : List Pattern -> Square -> Square
find_pattern patterns square =
    case patterns of
        [] ->
            Debug.crash "pattern not found"

        p :: tail ->
            case find_pattern_helper 0 p.from p.to square of
                Just r ->
                    r

                Nothing ->
                    find_pattern tail square


find_pattern_helper : Int -> Square -> Square -> Square -> Maybe Square
find_pattern_helper count from to square =
    if flat from == flat square then
        Just to
    else
        case count of
            0 ->
                find_pattern_helper (count + 1) (rotate from) to square

            1 ->
                find_pattern_helper (count + 1) (rotate from) to square

            2 ->
                find_pattern_helper (count + 1) (rotate from) to square

            3 ->
                find_pattern_helper (count + 1) (from |> rotate |> flip) to square

            4 ->
                find_pattern_helper (count + 1) (rotate from) to square

            5 ->
                find_pattern_helper (count + 1) (rotate from) to square

            6 ->
                find_pattern_helper (count + 1) (rotate from) to square

            _ ->
                Nothing


flip : Square -> Square
flip p =
    case p of
        Size2 m2 ->
            Size2 <| flip2 m2

        Size3 m3 ->
            Size3 <| flip3 m3

        _ ->
            Debug.crash "flip"


rotate : Square -> Square
rotate p =
    case p of
        Size2 m2 ->
            Size2 <| rotate2 m2

        Size3 m3 ->
            Size3 <| rotate3 m3

        _ ->
            Debug.crash "rotate"



{-
   A B => C A
   C D    D B
-}


rotate2 : M2 -> M2
rotate2 ( ( a, b ), ( c, d ) ) =
    ( ( c, a ), ( d, b ) )



{-
   A B => B A
   C D    D C
-}


flip2 : M2 -> M2
flip2 ( ( a, b ), ( c, d ) ) =
    ( ( b, a ), ( d, c ) )



{-
   A B C => G D A
   D E F    H E B
   G H I    I F C
-}


rotate3 : M3 -> M3
rotate3 ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) =
    ( ( g, d, a ), ( h, e, b ), ( i, f, c ) )



{-
   A B C => C B A
   D E F    F E D
   G H I    I H G
-}


flip3 : M3 -> M3
flip3 ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) =
    ( ( c, b, a ), ( f, e, d ), ( i, h, g ) )



--##/## => #.#/.#./#..
--.../.../... => ####/##../#.../#...


parse : String -> List Pattern
parse input =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map parse_line


parse_line : String -> Pattern
parse_line line =
    case String.words line of
        [ from, "=>", to ] ->
            { from = parse_square from
            , to = parse_square to
            }

        _ ->
            Debug.crash <| "parse_line <" ++ line ++ ">"


parse_square : String -> Square
parse_square input =
    case String.split "/" input of
        [ r0, r1 ] ->
            case ( String.toList r0, String.toList r1 ) of
                ( [ a, b ], [ c, d ] ) ->
                    Size2 ( ( a, b ), ( c, d ) )

                _ ->
                    Debug.crash <| "parse_square <" ++ input ++ ">"

        [ r0, r1, r2 ] ->
            case ( String.toList r0, String.toList r1, String.toList r2 ) of
                ( [ a, b, c ], [ d, e, f ], [ g, h, i ] ) ->
                    Size3 ( ( a, b, c ), ( d, e, f ), ( g, h, i ) )

                _ ->
                    Debug.crash <| "parse_square <" ++ input ++ ">"

        [ r0, r1, r2, r3 ] ->
            case ( String.toList r0, String.toList r1, String.toList r2, String.toList r3 ) of
                ( [ a, b, c, d ], [ e, f, g, h ], [ i, j, k, l ], [ m, n, o, p ] ) ->
                    Size4 ( ( a, b, c, d ), ( e, f, g, h ), ( i, j, k, l ), ( m, n, o, p ) )

                _ ->
                    Debug.crash <| "parse_square <" ++ input ++ ">"

        _ ->
            Debug.crash <| "parse_square <" ++ input ++ ">"


get : ( Int, Int ) -> Matrix a -> a
get location matrix =
    case Matrix.get location matrix of
        Just a ->
            a

        _ ->
            let
                dbg_matrix =
                    Debug.log "matrix" matrix

                dbg_location =
                    Debug.log "location" location
            in
            Debug.crash "bad index"


flat : Square -> List Char
flat square =
    case square of
        Size2 ( ( a, b ), ( c, d ) ) ->
            [ a, b, c, d ]

        Size3 ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) ->
            [ a, b, c, d, e, f, g, h, i ]

        Size4 ( ( a, b, c, d ), ( e, f, g, h ), ( i, j, k, l ), ( m, n, o, p ) ) ->
            [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ]
