--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Axis3d exposing
    ( direction
    , originPoint
    )

import Geometry.Types exposing (..)


originPoint : Axis3d units coordinates -> Point3d units coordinates
originPoint (Axis3d properties) =
    properties.originPoint


direction : Axis3d units coordinates -> Direction3d coordinates
direction (Axis3d properties) =
    properties.direction
