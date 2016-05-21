-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Bouncy exposing (Model, Ball, Box, Message(..), init, update, subscriptions)

import Random
import Time exposing (Time)
import AnimationFrame
import Shapes exposing (Sphere)
import Vector exposing (Vector)


-- MODEL


type alias Model =
    { ball : Ball
    , box : Box
    }


type alias Ball =
    { sphere : Sphere
    , velocity : Vector
    }


type alias Box =
    { at : Vector
    , halfDiagonal : Vector
    }


init :
    { radius : Float
    , halfDiagonal : Vector
    }
    -> Result String ( Model, Cmd Message )
init config =
    if config.halfDiagonal |> Vector.toList |> List.all ((<) config.radius) then
        let
            sphere =
                Sphere Vector.zero config.radius

            ball =
                Ball sphere Vector.zero

            box =
                Box Vector.zero config.halfDiagonal
        in
            Ok <| ( Model ball box, resetVelocity 0.7 )
    else
        Err "The ball can't fit into the box!"


resetVelocity : Float -> Cmd Message
resetVelocity speed =
    let
        buildMessage x y z =
            { ballVelocity = Vector.scale speed <| Vector.unit <| Vector x y z }
                |> Reset

        component =
            Random.float -1 1
    in
        Random.map3 buildMessage component component component
            |> Random.generate identity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions _ =
    AnimationFrame.diffs (\dt -> Integrate { dt = dt })



-- UPDATE


type Message
    = Reset { ballVelocity : Vector }
    | Integrate { dt : Time }


update : Message -> Model -> Model
update msg model =
    case msg of
        Reset { ballVelocity } ->
            model |> resetBall ballVelocity

        Integrate { dt } ->
            model |> moveBall dt


resetBall : Vector -> Model -> Model
resetBall newVelocity model =
    let
        sphere =
            Sphere Vector.zero model.ball.sphere.radius

        ball =
            Ball sphere newVelocity
    in
        { model | ball = ball }


moveBall : Time -> Model -> Model
moveBall dt ({ ball, box } as model) =
    let
        ( bouncedBall, collisionAt ) =
            nextCollision box ball
    in
        if collisionAt < dt then
            moveBall (dt - collisionAt) { model | ball = bouncedBall }
        else
            { model | ball = ball |> dumbIntegrate dt }


nextCollision : Box -> Ball -> ( Ball, Time )
nextCollision box ball =
    let
        ( inBoxAt, radius ) =
            ( ball.sphere.at `Vector.sub` box.at
            , ball.sphere.radius
            )

        innerBox =
            { box | halfDiagonal = box.halfDiagonal `Vector.sub` Vector radius radius radius }

        collisionIn rMax r v f =
            if (f v) < 0 then
                (negate (f rMax) - f r) / f v
            else
                (f rMax - f r) / f v

        collisionInComponent =
            collisionIn innerBox.halfDiagonal ball.sphere.at ball.velocity

        minBy conv one other =
            if conv one < conv other then
                one
            else
                other

        flipBy : (Vector -> Float) -> (Float -> Vector -> Vector) -> Vector -> Vector
        flipBy get set v =
            v |> set (v |> get |> negate)

        minByTime =
            minBy snd

        ( flipVelocity, time ) =
            ( flipBy .x (\x v -> { v | x = x })
            , collisionInComponent .x
            )
                `minByTime` ( flipBy .y (\y v -> { v | y = y })
                            , collisionInComponent .y
                            )
                `minByTime` ( flipBy .z (\z v -> { v | z = z })
                            , collisionInComponent .z
                            )

        flipBall ball =
            { ball | velocity = ball.velocity |> flipVelocity }
    in
        ( ball |> dumbIntegrate time |> flipBall
        , time
        )


dumbIntegrate : Time -> Ball -> Ball
dumbIntegrate time ball =
    let
        shift =
            Vector.scale time ball.velocity

        at =
            ball.sphere.at `Vector.add` shift

        sphere =
            Sphere at ball.sphere.radius
    in
        { ball | sphere = sphere }
