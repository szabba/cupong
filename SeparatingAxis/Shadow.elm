-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module SeparatingAxis.Shadow exposing (Shadow, emptyAt, distance, fixDirection, shiftToStartOf, extendTo)

import Tuple2


type alias Distance =
    Float


type alias Shadow =
    { from : Float, delta : Float }


emptyAt : Float -> Shadow
emptyAt at =
    Shadow at at


distance : Shadow -> Shadow -> Distance
distance =
    curry
        <| \shadows ->
            let
                difference ( { delta }, b ) =
                    if delta < b.from then
                        b.from - delta
                    else
                        0
            in
                shadows
                    |> Tuple2.mapBoth fixDirection
                    |> tupleSortBy .from
                    |> (\shds ->
                            shds |> Tuple2.mapBoth (shiftToStartOf <| fst shds)
                       )
                    |> difference


shiftToStartOf : Shadow -> Shadow -> Shadow
shiftToStartOf reference shadow =
    { shadow | from = shadow.from - reference.from }


fixDirection : Shadow -> Shadow
fixDirection ({ from, delta } as shadow) =
    if delta < 0 then
        Shadow (from + delta) -delta
    else
        shadow


extendTo : Float -> Shadow -> Shadow
extendTo point ({ from, delta } as shadow) =
    if point < from then
        { shadow | from = point }
    else if from + delta < point then
        { shadow | delta = point - from }
    else
        shadow


tupleSortBy : (a -> comparable) -> ( a, a ) -> ( a, a )
tupleSortBy f ( a, b ) =
    if f a > f b then
        ( b, a )
    else
        ( a, b )
