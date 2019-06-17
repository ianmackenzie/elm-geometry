--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Direction2d exposing
    ( perpendicularTo
    , reverse
    , unsafeFromComponents
    , xComponent
    , yComponent
    )

import Geometry.Types exposing (..)
import Quantity exposing (Quantity)


unsafeFromComponents : Float -> Float -> Direction2d coordinates
unsafeFromComponents x y =
    Direction2d ( x, y )


xComponent : Direction2d coordinates -> Float
xComponent (Direction2d ( x, y )) =
    x


yComponent : Direction2d coordinates -> Float
yComponent (Direction2d ( x, y )) =
    y


reverse : Direction2d coordinates -> Direction2d coordinates
reverse (Direction2d ( x, y )) =
    Direction2d ( -x, -y )


perpendicularTo : Direction2d coordinates -> Direction2d coordinates
perpendicularTo (Direction2d ( x, y )) =
    Direction2d ( -y, x )
