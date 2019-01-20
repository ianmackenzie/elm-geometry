--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Frame3d exposing
    ( originPoint
    , xDirection
    , yDirection
    , zDirection
    )

import Geometry.Types exposing (..)


originPoint : Frame3d units coordinates1 coordinates2 -> Point3d units coordinates1
originPoint (Frame3d properties) =
    properties.originPoint


xDirection : Frame3d units coordinates1 coordinates2 -> Direction3d coordinates1
xDirection (Frame3d properties) =
    properties.xDirection


yDirection : Frame3d units coordinates1 coordinates2 -> Direction3d coordinates1
yDirection (Frame3d properties) =
    properties.yDirection


zDirection : Frame3d units coordinates1 coordinates2 -> Direction3d coordinates1
zDirection (Frame3d properties) =
    properties.zDirection
