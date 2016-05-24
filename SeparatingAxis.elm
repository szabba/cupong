-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module SeparatingAxis exposing (Body(..), Axis, Distance, find)

import Shapes exposing (Sphere, Face)
import SeparatingAxis.Shadow as Shadow exposing (Shadow)
import Vector exposing (Vector)


type alias Axis =
    Vector


type alias Distance =
    Float


type Body
    = Ball Sphere
    | Faces (List Face)


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
            shadowsAlong axis
    in
        List.map2 Shadow.distance (shadowsOf oneBody) (shadowsOf otherBody)
            |> List.sort
            |> List.head
            |> Maybe.withDefault 0


shadowsAlong : Axis -> Body -> List Shadow
shadowsAlong axis body =
    case body of
        Ball { at, radius } ->
            { from = Vector.dot at axis - radius
            , delta = 2 * radius
            }
                |> flip (::) []

        Faces faces ->
            faces |> List.map (faceShadowAlong axis)


faceShadowAlong : Axis -> Face -> Shadow
faceShadowAlong axis { at, spans } =
    let
        alongAxis =
            Vector.dot (Vector.unit axis)

        init =
            Shadow.emptyAt (alongAxis at)

        ( a, b ) =
            spans
    in
        [ at `Vector.add` a
        , at `Vector.add` b
        , at `Vector.add` a `Vector.add` b
        ]
            |> List.map alongAxis
            |> List.foldl Shadow.extendTo init
