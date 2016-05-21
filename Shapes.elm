-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Shapes exposing (..)

import Vector exposing (Vector)


type alias Sphere =
    { at : Vector, radius : Float }


type alias Face =
    { at : Vector
    , spans : ( Vector, Vector )
    }


type alias Line =
    { from : Vector, to : Vector }
