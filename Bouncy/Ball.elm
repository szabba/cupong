-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Bouncy.Ball exposing (Ball)

import Vector exposing (Vector)


type alias Ball =
    { at : Vector
    , radius : Float
    , velocity : Vector
    }
