--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.SketchPlane3d exposing
    ( originPoint
    , unsafe
    , xDirection
    , yDirection
    )

import Geometry.Types exposing (..)


unsafe : { originPoint : Point3d units coordinates1, xDirection : Direction3d coordinates1, yDirection : Direction3d coordinates1 } -> SketchPlane3d units coordinates1 coordinates2
unsafe properties =
    SketchPlane3d properties


originPoint : SketchPlane3d units coordinates1 coordinates2 -> Point3d units coordinates1
originPoint (SketchPlane3d properties) =
    properties.originPoint


xDirection : SketchPlane3d units coordinates1 coordinates2 -> Direction3d coordinates1
xDirection (SketchPlane3d properties) =
    properties.xDirection


yDirection : SketchPlane3d units coordinates1 coordinates2 -> Direction3d coordinates1
yDirection (SketchPlane3d properties) =
    properties.yDirection
