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
    , unsafeFromComponents
    )

import Geometry.Types exposing (..)
import Quantity exposing (Quantity)


unsafeFromComponents : ( Float, Float ) -> Direction2d coordinates
unsafeFromComponents givenComponents =
    Direction2d givenComponents


components : Direction2d coordinates -> ( Float, Float )
components (Direction2d directionComponents) =
    directionComponents


reverse : Direction2d coordinates -> Direction2d coordinates
reverse direction =
    let
        ( x, y ) =
            components direction
    in
    unsafeFromComponents ( -x, -y )


perpendicularTo : Direction2d coordinates -> Direction2d coordinates
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    unsafeFromComponents ( -y, x )
