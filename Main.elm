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

        ( model, cmd, _ ) =
            Bouncy.init { radius = 10, halfDiagonal = Vector 300 300 300 }
    in
        (model |> withEmptyWindow)
            ! [ cmd |> Cmd.map BouncyMsg
              , Task.perform (\_ -> Debug.crash "initial window size") Resize Window.size
              ]



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
        [ viewBox bouncy.box
        , viewSphere bouncy.ball
        ]


wrapWindow : WindowSize -> List (Svg msg) -> Svg msg
wrapWindow { width, height } elements =
    let
        baseAttributes =
            [ ASvg.version "1.1"
            , ASvg.baseProfile "full"
            , ASvg.width <| toString <| width
            , ASvg.height <| toString <| height
            , AHtml.attribute "xmlns" "http://www.w3.org/2000/svg"
            ]

        size =
            [ ASvg.width <| toString <| width
            , ASvg.height <| toString <| height
            ]

        styling =
            [ AHtml.style
                [ (,) "background" background
                , (,) "display" "block"
                ]
            ]

        centering =
            [ ASvg.transform <| "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")" ]
    in
        Svg.svg (baseAttributes ++ size ++ styling) [ Svg.g centering elements ]


viewSphere : Bouncy.Ball -> Svg msg
viewSphere ball =
    let
        ( cx, cy ) =
            ( .x, .y )
                |> both ((|>) ball.sphere.at >> round >> toString)
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


viewBox : Bouncy.Box -> Svg msg
viewBox box =
    let
        param f attr component =
            attr <| toString <| f <| abs <| component <| box.halfDiagonal

        start =
            param negate

        dim =
            param ((*) 2)

        shape =
            [ start ASvg.x .x, start ASvg.y .y, dim ASvg.width .x, dim ASvg.height .y ]

        style =
            [ stroke, strokeWidth, ASvg.fillOpacity "0" ]
    in
        Svg.rect (shape ++ style) []


both : (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) =
    ( f x, f y )


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
