--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Axis2d exposing
    ( direction
    , originPoint
    )

import Geometry.Types exposing (..)


originPoint : Axis2d -> Point2d
originPoint (Axis2d properties) =
    properties.originPoint


direction : Axis2d -> Direction2d
direction (Axis2d properties) =
    properties.direction
