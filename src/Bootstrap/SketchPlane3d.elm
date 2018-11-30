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


unsafe : { originPoint : Point3d units coordinates, xDirection : Direction3d coordinates, yDirection : Direction3d coordinates } -> SketchPlane3d units coordinates {}
unsafe properties =
    SketchPlane3d properties


originPoint : SketchPlane3d units coordinates defines -> Point3d units coordinates
originPoint (SketchPlane3d properties) =
    properties.originPoint


xDirection : SketchPlane3d units coordinates defines -> Direction3d coordinates
xDirection (SketchPlane3d properties) =
    properties.xDirection


yDirection : SketchPlane3d units coordinates defines -> Direction3d coordinates
yDirection (SketchPlane3d properties) =
    properties.yDirection
