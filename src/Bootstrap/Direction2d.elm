--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Direction2d exposing
    ( components
    , perpendicularTo
    , reverse
    , unsafe
    )

import Geometry.Types exposing (..)
import Quantity exposing (Quantity)


unsafe : ( Float, Float ) -> Direction2d (Coordinates2d system units)
unsafe givenComponents =
    Direction2d givenComponents


components : Direction2d (Coordinates2d system units) -> ( Float, Float )
components (Direction2d directionComponents) =
    directionComponents


reverse : Direction2d (Coordinates2d system units) -> Direction2d (Coordinates2d system units)
reverse direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -x, -y )


perpendicularTo : Direction2d (Coordinates2d system units) -> Direction2d (Coordinates2d system units)
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -y, x )
