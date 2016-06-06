-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Bouncy exposing (Model, init, Message, update, integrate)

import Random
import Time exposing (Time)
import Tuple3
import Vector exposing (Vector)
import Bouncy.Box as Box exposing (Box)
import Bouncy.Ball as Ball exposing (Ball)


-- MODEL


type alias Model =
    { ball : Ball
    , box : Box
    }


type Error
    = BallWontFit


type alias Distance =
    Float


init :
    { radius : Float
    , halfDiagonal : Vector
    }
    -> ( Model, Cmd Message, List Error )
init config =
    let
        errors =
            [ ( config.halfDiagonal
                    |> Vector.toList
                    |> List.all ((<) config.radius)
              , BallWontFit
              )
            ]
                |> List.filter fst
                |> List.map snd

        safeConfig =
            if List.isEmpty errors then
                config
            else
                { radius = 10
                , halfDiagonal = Vector 300 300 300
                }

        ( model, cmd ) =
            unsafeInit config
    in
        ( model, cmd, errors )


unsafeInit :
    { radius : Float
    , halfDiagonal : Vector
    }
    -> ( Model, Cmd Message )
unsafeInit { radius, halfDiagonal } =
    let
        ball =
            { at = Vector.zero
            , radius = radius
            , velocity = Vector.zero
            }

        box =
            { at = Vector.zero
            , halfDiagonal = halfDiagonal
            }
    in
        Model ball box ! [ resetVelocity 0.3 ]



-- UPDATE


type Message
    = Reset { ballVelocity : Vector }


update : Message -> Model -> Model
update msg model =
    case msg of
        Reset { ballVelocity } ->
            model |> resetBall ballVelocity


integrate : Time -> Model -> Model
integrate =
    moveBall


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


resetBall : Vector -> Model -> Model
resetBall newVelocity ({ ball } as model) =
    let
        newBall =
            { ball | velocity = newVelocity }
    in
        { model | ball = newBall }


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
nextCollision box ({ radius } as ball) =
    let
        inBoxAt =
            ball.at `Vector.sub` box.at

        innerBox =
            { box | halfDiagonal = box.halfDiagonal `Vector.sub` Vector radius radius radius }

        along =
            collision1D
                { ranges = innerBox.halfDiagonal
                , at = ball.at
                , velocity = ball.velocity
                }

        minByTime =
            minBy snd

        ( flipVector, time ) =
            along Vector.x `minByTime` along Vector.y `minByTime` along Vector.z

        flipBall ball =
            { ball | velocity = ball.velocity |> flipVector }
    in
        ( ball |> dumbIntegrate time |> flipBall, time )


collision1D : { ranges : Vector, at : Vector, velocity : Vector } -> Vector -> ( Vector -> Vector, Distance )
collision1D system axis =
    let
        ( xMax, x, vx ) =
            system
                |> tuple3.repeat
                |> Tuple3.mapEach .ranges .at .velocity
                |> Tuple3.mapAll (Vector.dot <| Vector.unit axis)

        flip =
            Vector.decomposeAlong axis
                >> \( prj, rej ) -> Vector.negate prj `Vector.add` rej
    in
        ( flip
        , if vx < 0 then
            (-xMax - x) / vx
          else
            (xMax - x) / vx
        )


dumbIntegrate : Time -> Ball -> Ball
dumbIntegrate time ball =
    let
        shift =
            Vector.scale time ball.velocity

        at =
            ball.at `Vector.add` shift
    in
        { ball | at = at }


minBy : (a -> comparable) -> a -> a -> a
minBy conv one other =
    if conv one < conv other then
        one
    else
        other


tuple3 : { repeat : a -> ( a, a, a ) }
tuple3 =
    { repeat = \v -> ( v, v, v ) }
