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
