-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import AnimationFrame
import Html as H exposing (Attribute)
import Html.App as App
import Html.Attributes as HA
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Task
import Time exposing (Time)
import Window
import Bouncy
import Bouncy.Box as Box exposing (Box)
import Bouncy.Ball as Ball exposing (Ball)
import Vector exposing (Vector)


main : Program Never
main =
    App.program
        { init = init
        , update = \msg model -> update msg model ! []
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { window : WindowSize
    , bouncy : Bouncy.Model
    , backScale : Float
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


init : ( Model, Cmd Message )
init =
    let
        withEmptyWindow bouncy =
            { bouncy = bouncy
            , backScale = 1 / 2
            , window =
                { width = 0
                , height = 0
                }
            }

        ( model, cmd, _ ) =
            Bouncy.init
                { radius = 25
                , halfDiagonal = Vector 400 300 400
                }
    in
        (model |> withEmptyWindow)
            ! [ cmd |> Cmd.map BouncyMsg
              , Window.size
                    |> Task.perform (\_ -> Debug.crash "initial window size") Resize
              ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Integrate
        ]



-- UPDATE


type Message
    = Integrate Time
    | Resize { width : Int, height : Int }
    | BouncyMsg Bouncy.Message


update : Message -> Model -> Model
update msg model =
    case msg of
        Resize window ->
            { model | window = window }

        Integrate dt ->
            { model | bouncy = Bouncy.integrate dt model.bouncy }

        BouncyMsg msg ->
            { model | bouncy = Bouncy.update msg model.bouncy }



-- VIEW


view : Model -> Svg msg
view ({ window, bouncy, backScale } as model) =
    [ box model bouncy.box
    , ball model bouncy.box bouncy.ball
    ]
        |> fullscreen window


fullscreen : WindowSize -> List (Svg msg) -> Svg msg
fullscreen { width, height } elements =
    let
        baseAttributes =
            [ SA.version "1.1"
            , SA.baseProfile "full"
            , SA.width <| toString <| width
            , SA.height <| toString <| height
            , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
            ]

        size =
            [ SA.width <| toString <| width
            , SA.height <| toString <| height
            ]

        styling =
            [ HA.style
                [ (,) "background" background
                , (,) "display" "block"
                ]
            ]

        centering =
            [ SA.transform <| "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")" ]
    in
        S.svg (baseAttributes ++ size ++ styling) [ S.g centering elements ]


ball : { a | backScale : Float } -> Box -> Ball -> Svg msg
ball { backScale } box ball =
    let
        zMax =
            abs box.halfDiagonal.z

        -- scale z = a * z + b
        -- scale (-zMax) = 1
        -- scale zMax = backScale
        scale =
            let
                ( a, b ) =
                    ( (backScale - 1) / (2 * zMax), (backScale + 1) / 2 )
            in
                a * ball.at.z + b

        ( cx, cy ) =
            ( .x, .y )
                |> both ((|>) ball.at >> (*) scale >> round >> toString)

        geometry =
            [ SA.cx cx
            , SA.cy cy
            , SA.r <| toString <| round <| (*) scale <| ball.radius
            ]

        style =
            [ stroke, strokeWidth, fill ]
    in
        S.circle (geometry ++ style) []


box : { a | backScale : Float } -> Box -> Svg msg
box { backScale } box =
    let
        rect scale =
            { width = round <| scale * 2 * abs box.halfDiagonal.x
            , height = round <| scale * 2 * abs box.halfDiagonal.y
            }

        ( front, back ) =
            ( rect 1, rect backScale )

        rects =
            [ front, back ] |> List.map centeredRect

        line xSign ySign =
            S.line
                [ SA.x1 <| toString <| xSign <| front.width // 2
                , SA.y1 <| toString <| ySign <| front.height // 2
                , SA.x2 <| toString <| xSign <| back.width // 2
                , SA.y2 <| toString <| ySign <| back.height // 2
                , stroke
                , strokeWidth
                ]
                []

        lines =
            [ line identity identity
            , line identity negate
            , line negate identity
            , line negate negate
            ]
    in
        S.g [] (rects ++ lines)


centeredRect : { width : Int, height : Int } -> Svg msg
centeredRect { width, height } =
    let
        geometry =
            [ SA.x <| toString <| negate <| flip (//) 2 <| width
            , SA.y <| toString <| negate <| flip (//) 2 <| height
            , SA.width <| toString <| width
            , SA.height <| toString <| height
            ]

        style =
            [ stroke
            , strokeWidth
            , SA.fillOpacity "0"
            ]
    in
        S.rect (geometry ++ style) []


both : (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) =
    ( f x, f y )


fill : Attribute msg
fill =
    SA.fill background


stroke : Attribute msg
stroke =
    SA.stroke "#000000"


strokeWidth : Attribute msg
strokeWidth =
    SA.strokeWidth <| toString 1


background : String
background =
    "#FFFFFF"
