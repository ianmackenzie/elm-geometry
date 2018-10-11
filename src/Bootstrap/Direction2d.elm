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


unsafe : ( Quantity number units, Quantity number units ) -> Direction2d (Coordinates2d system number units)
unsafe givenComponents =
    Direction2d (Coordinates2d givenComponents)


components : Direction2d (Coordinates2d system number units) -> ( Quantity number units, Quantity number units )
components (Direction2d (Coordinates2d components_)) =
    components_


reverse : Direction2d (Coordinates2d system number units) -> Direction2d (Coordinates2d system number units)
reverse direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( Quantity.negate x, Quantity.negate y )


perpendicularTo : Direction2d (Coordinates2d system number units) -> Direction2d (Coordinates2d system number units)
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( Quantity.negate y, x )
