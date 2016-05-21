-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import Html exposing (Attribute)
import Html.App as App
import Html.Attributes as AHtml
import Svg exposing (Svg)
import Svg.Attributes as ASvg
import Task
import Window
import Bouncy
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
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


init : ( Model, Cmd Message )
init =
    let
        withEmptyWindow bouncy =
            { window =
                { width = 0
                , height = 0
                }
            , bouncy = bouncy
            }
    in
        case Bouncy.init { radius = 10, halfDiagonal = Vector 300 300 300 } of
            Ok ( model, cmd ) ->
                (model |> withEmptyWindow)
                    ! [ cmd |> Cmd.map BouncyMsg
                      , Task.perform (\_ -> Debug.crash "initial window size") Resize Window.size
                      ]

            Err msg ->
                Debug.crash msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Bouncy.subscriptions model.bouncy |> Sub.map BouncyMsg
        ]



-- UPDATE


type Message
    = Resize { width : Int, height : Int }
    | BouncyMsg Bouncy.Message


update : Message -> Model -> Model
update msg model =
    case msg of
        Resize window ->
            { model | window = window }

        BouncyMsg msg ->
            { model | bouncy = Bouncy.update msg model.bouncy }



-- VIEW


view : Model -> Svg msg
view { window, bouncy } =
    wrapWindow window
        [ viewSphere window bouncy.ball ]


wrapWindow : WindowSize -> List (Svg msg) -> Svg msg
wrapWindow { width, height } =
    Svg.svg
        [ ASvg.version "1.1"
        , ASvg.baseProfile "full"
        , ASvg.width <| toString <| width
        , ASvg.height <| toString <| height
        , AHtml.attribute "xmlns" "http://www.w3.org/2000/svg"
        , AHtml.style
            [ (,) "background" background
            , (,) "display" "block"
            ]
        ]


viewSphere : WindowSize -> Bouncy.Ball -> Svg msg
viewSphere { width, height } ball =
    let
        both f ( x, y ) =
            ( f x, f y )

        ( cx, cy ) =
            ( width, height )
                |> both (toFloat >> flip (/) 2)
                |> (\( x, y ) -> ( x + ball.sphere.at.x, y + ball.sphere.at.y ))
                |> both (round >> toString)
    in
        Svg.circle
            [ ASvg.cx cx
            , ASvg.cy cy
            , ASvg.r <| toString <| round <| ball.sphere.radius
            , stroke
            , strokeWidth
            , fill
            ]
            []


centeredSphere : WindowSize -> Svg msg
centeredSphere { width, height } =
    Svg.circle
        [ ASvg.cx <| toString <| width // 2
        , ASvg.cy <| toString <| height // 2
        , ASvg.r <| toString <| flip (//) 20 <| min width height
        , stroke
        , strokeWidth
        , fill
        ]
        []


fill : Attribute msg
fill =
    ASvg.fill background


stroke : Attribute msg
stroke =
    ASvg.stroke "#000000"


strokeWidth : Attribute msg
strokeWidth =
    ASvg.strokeWidth <| toString 5


background : String
background =
    "#FFFFFF"
