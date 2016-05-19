-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module SeparatingAxis exposing (Body(..), Axis, Distance, Sphere, Face, find)

import Vector exposing (Vector)


type alias Axis =
    Vector


type alias Distance =
    Float


type Body
    = Ball Sphere
    | Faces (List Face)


type alias Sphere =
    { at : Vector, radius : Float }


type alias Face =
    { at : Vector
    , spans : ( Vector, Vector )
    }


{-| Returns a list of all the axes that could be separating along with the
current distances between the objects along them.

-}
find : Body -> Body -> List ( Axis, Distance )
find one other =
    candidateAxes one other
        |> List.map (\axis -> ( axis, distanceAlong axis one other ))
        |> List.sortBy snd


candidateAxes : Body -> Body -> List Axis
candidateAxes one other =
    case ( one, other ) of
        ( Ball one, Ball other ) ->
            [ other.at `Vector.sub` one.at ]

        ( Faces one, Faces other ) ->
            List.map faceAxis one ++ List.map faceAxis other

        ( Ball _, Faces faces ) ->
            List.map faceAxis faces

        ( Faces faces, Ball _ ) ->
            candidateAxes other one


faceAxis : Face -> Axis
faceAxis { spans } =
    uncurry Vector.cross spans


distanceAlong : Axis -> Body -> Body -> Distance
distanceAlong axis oneBody otherBody =
    let
        shadowsOf =
            shadows axis
    in
        List.map2 distance (shadowsOf oneBody) (shadowsOf otherBody)
            |> List.sort
            |> List.head
            |> Maybe.withDefault 0


shadows : Axis -> Body -> List Shadow
shadows axis body =
    case body of
        Ball { at, radius } ->
            { from = Vector.dot at axis - radius
            , delta = 2 * radius
            }
                |> flip (::) []

        Faces faces ->
            faces |> List.map (faceShadow axis)


faceShadow : Axis -> Face -> Shadow
faceShadow axis { at, spans } =
    let
        ( a, b ) =
            spans

        addPoint point ({ from, delta } as shadow) =
            if point < from then
                { shadow | from = point }
            else if from + delta < point then
                { shadow | delta = point - from }
            else
                shadow

        init =
            Vector.dot axis at
    in
        [ at `Vector.add` a
        , at `Vector.add` b
        , at `Vector.add` a `Vector.add` b
        ]
            |> List.map (Vector.dot axis)
            |> List.foldl addPoint (Shadow init init)


distance : Shadow -> Shadow -> Distance
distance one other =
    let
        fixDirections ( a, b ) =
            ( a |> fixShadowDirection, b |> fixShadowDirection )

        sort ( a, b ) =
            if a.from > b.from then
                ( a, b )
            else
                ( b, a )

        shift ( a, b ) =
            ( { a | from = 0 }, { b | from = b.from - a.from } )

        difference ( { delta }, b ) =
            if delta < b.from then
                b.from - delta
            else
                0
    in
        ( one, other )
            |> fixDirections
            |> sort
            |> shift
            |> difference


fixShadowDirection : Shadow -> Shadow
fixShadowDirection ({ from, delta } as shadow) =
    if delta < 0 then
        Shadow (from + delta) -delta
    else
        shadow


type alias Shadow =
    { from : Float, delta : Float }
