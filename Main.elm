-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import Html.App as App
import Html.Attributes as AHtml
import Svg exposing (Svg)
import Svg.Attributes as ASvg
import Task
import Window


main : Program Never
main =
    App.program
        { init =
            ( init
            , Task.perform (\_ -> Debug.crash "initial window size") Resize Window.size
            )
        , update = \msg model -> update msg model ! []
        , view = view
        , subscriptions = always <| Window.resizes Resize
        }



-- MODEL


type alias Model =
    { width : Int, height : Int }


init : Model
init =
    { width = 0
    , height = 0
    }



-- UPDATE


type Message
    = Resize { width : Int, height : Int }


update : Message -> Model -> Model
update msg model =
    case msg of
        Resize { width, height } ->
            { model | width = width, height = height }



-- VIEW


view : Model -> Svg msg
view model =
    window model []


window : Model -> List (Svg msg) -> Svg msg
window { width, height } =
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


background : String
background =
    "#F5DEB3"
