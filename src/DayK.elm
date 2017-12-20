module DayK exposing (main)

import DayKInput exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)
import Set exposing (Set)


main : Html msg
main =
    Html.text "Check console!"


type alias Particle =
    { id : Int
    , p : ( Int, Int, Int )
    , v : ( Int, Int, Int )
    , a : ( Int, Int, Int )
    }


distance : ( Int, Int, Int ) -> Int
distance ( px, py, pz ) =
    abs px + abs py + abs pz


parse : List Particle
parse =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map String.words
        |> List.indexedMap (,)
        |> List.map parse_words


step_while_in : List Particle -> List Particle
step_while_in particles =
    let
        all_out =
            particles
                |> List.all (\p -> p.phase == Out)
    in
    if all_out then
        particles
    else
        particles
            |> List.map step_particle
            |> step_while_in


brute_force : Int
brute_force =
    run_for parse 0


run_for : List Particle -> Int -> Int
run_for particles counter =
    let
        particles_ =
            particles
                |> explode_collisions
                |> List.map step_particle

        dbg_2 =
            particles
                |> List.length
                |> toString
                |> Debug.log (toString counter)

        -- dbg_1 =
        --     if counter % 100 == 0 then
        --         particles
        --             |> List.sortBy (\p -> distance p.p)
        --             |> List.take 5
        --             |> List.map (\p -> ( p.id, distance p.p ))
        --             |> toString
        --             |> Debug.log (toString counter)
        --     else
        --         ""
    in
    if counter > 1000 then
        counter
    else
        run_for particles_ (counter + 1)


explode_collisions : List Particle -> List Particle
explode_collisions particles =
    particles
        |> group_by_distance Dict.empty
        |> Dict.values
        |> List.map (\list -> drop_collision list list Set.empty)
        |> List.concat


group_by_distance : Dict Int (List Particle) -> List Particle -> Dict Int (List Particle)
group_by_distance dict particles =
    case particles of
        [] ->
            dict

        p :: tail ->
            let
                d =
                    distance p.p

                dict_ =
                    dict
                        |> Dict.get d
                        |> Maybe.map (\list -> Dict.insert d (p :: list) dict)
                        |> Maybe.withDefault (Dict.insert d [ p ] dict)
            in
            group_by_distance dict_ tail


drop_collision : List Particle -> List Particle -> Set Int -> List Particle
drop_collision particles all_particles ids =
    case particles of
        [] ->
            all_particles
                |> List.filter (\p -> not (Set.member p.id ids))

        p :: tail ->
            all_particles
                |> find (\p1 -> p1.id /= p.id && p1.p == p.p)
                |> Maybe.map (\_ -> Set.insert p.id ids)
                |> Maybe.withDefault ids
                |> drop_collision tail all_particles


step_particle : Particle -> Particle
step_particle particle =
    let
        ( px, py, pz ) =
            particle.p

        ( vx, vy, vz ) =
            particle.v

        ( ax, ay, az ) =
            particle.a

        ( vx_, vy_, vz_ ) =
            ( vx + ax, vy + ay, vz + az )

        ( px_, py_, pz_ ) =
            ( px + vx_, py + vy_, pz + vz_ )

        dist =
            distance particle.p

        dist_ =
            distance ( px_, py_, pz_ )
    in
    { particle
        | p = ( px_, py_, pz_ )
        , v = ( vx_, vy_, vz_ )
    }


parse_words : ( Int, List String ) -> Particle
parse_words ( id, words ) =
    case words of
        [ px, py, pz, vx, vy, vz, ax, ay, az ] ->
            let
                p =
                    ( toInt px, toInt py, toInt pz )

                v =
                    ( toInt vx, toInt vy, toInt vz )

                a =
                    ( toInt ax, toInt ay, toInt az )
            in
            { id = id
            , p = p
            , v = v
            , a = a
            }

        _ ->
            Debug.crash "parse_words"


toInt : String -> Int
toInt v =
    case String.toInt v of
        Ok v ->
            v

        Err _ ->
            Debug.crash "toInt"


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] ->
            Nothing

        a :: tail ->
            if f a then
                Just a
            else
                find f tail
