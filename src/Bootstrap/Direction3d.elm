--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Direction3d exposing
    ( unsafeFromComponents
    , xComponent
    , yComponent
    , zComponent
    )

import Geometry.Types exposing (..)


unsafeFromComponents : Float -> Float -> Float -> Direction3d coordinates
unsafeFromComponents x y z =
    Direction3d ( x, y, z )


xComponent : Direction3d coordinates -> Float
xComponent (Direction3d ( x, y, z )) =
    x


yComponent : Direction3d coordinates -> Float
yComponent (Direction3d ( x, y, z )) =
    y


zComponent : Direction3d coordinates -> Float
zComponent (Direction3d ( x, y, z )) =
    z
