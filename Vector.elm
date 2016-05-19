-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Vector exposing (..)


type alias Vector =
    { x : Float, y : Float, z : Float }


zero : Vector
zero =
    Vector 0 0 0


rejectFrom : Vector -> Vector -> Vector
rejectFrom axis v =
    decomposeAlong axis v |> snd


projectTo : Vector -> Vector -> Vector
projectTo axis v =
    decomposeAlong axis v |> fst


decomposeAlong : Vector -> Vector -> ( Vector, Vector )
decomposeAlong axis v =
    let
        axis =
            unit axis

        projection =
            scale (dot axis v) axis

        rejection =
            sub v projection
    in
        ( projection, rejection )


length : Vector -> Float
length a =
    sqrt (dot a a)


unit : Vector -> Vector
unit v =
    scale (1 / length v) v


sum : List Vector -> Vector
sum vs =
    vs |> List.foldl add zero


add : Vector -> Vector -> Vector
add a b =
    Vector (a.x + b.x) (a.y + b.y) (a.z + b.z)


negate : Vector -> Vector
negate v =
    scale -1 v


scale : Float -> Vector -> Vector
scale s { x, y, z } =
    Vector (s * x) (s * y) (s * z)


sub : Vector -> Vector -> Vector
sub a b =
    add a (negate b)


dot : Vector -> Vector -> Float
dot a b =
    [ .x, .y, .z ]
        |> List.map (\f -> f a * f b)
        |> List.sum


cross : Vector -> Vector -> Vector
cross a b =
    let
        ( x, y, z ) =
            ( a.y * b.z - a.z * b.y
            , a.z * b.x - a.x * b.z
            , a.x * b.y - a.y * b.x
            )
    in
        Vector x y z
