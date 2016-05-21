-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Shapes exposing (..)

import Vector exposing (Vector)


type alias Sphere =
    { at : Vector, radius : Float }


type alias Face =
    { at : Vector
    , spans : ( Vector, Vector )
    }


type alias Plane =
    { at : Vector
    , normal : Vector
    }


type alias Line =
    { from : Vector, to : Vector }


planeLineIntersection : Plane -> Line -> Maybe Vector
planeLineIntersection { at, normal } { from, to } =
    let
        both f ( x, y ) =
            ( f x, f y )

        endsOnOppositeSides =
            let
                ( localFrom, localTo ) =
                    ( from `Vector.sub` at
                    , to `Vector.sub` at
                    )
            in
                (localFrom `Vector.dot` normal) * (localTo `Vector.dot` normal) < 0
    in
        if endsOnOppositeSides then
            let
                line =
                    to `Vector.sub` from

                ( toPrj, fromPrj ) =
                    ( to, from )
                        |> both (Vector.projectTo normal >> Vector.length)

                intersection =
                    line
                        |> Vector.scale (fromPrj / (fromPrj + toPrj))
                        |> Vector.add from
            in
                Just intersection
        else
            Nothing


cutLine : Plane -> Line -> Maybe { front : Line, back : Line }
cutLine ({ at, normal } as plane) ({ from, to } as line) =
    let
        both f ( x, y ) =
            ( f x, f y )

        ( localTo, localFrom ) =
            ( to, from )
                |> both (flip Vector.sub at)

        withIntersection intersection =
            if localTo `Vector.dot` normal > 0 then
                Just
                    { front = { line | from = intersection }
                    , back = { line | to = intersection }
                    }
            else if localFrom `Vector.dot` normal > 0 then
                Just
                    { front = { line | to = intersection }
                    , back = { line | from = intersection }
                    }
            else
                Nothing
    in
        planeLineIntersection plane line
            |> flip Maybe.andThen withIntersection
