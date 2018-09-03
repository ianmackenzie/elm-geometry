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


unsafe : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d } -> SketchPlane3d
unsafe =
    SketchPlane3d


originPoint : SketchPlane3d -> Point3d
originPoint (SketchPlane3d properties) =
    properties.originPoint


xDirection : SketchPlane3d -> Direction3d
xDirection (SketchPlane3d properties) =
    properties.xDirection


yDirection : SketchPlane3d -> Direction3d
yDirection (SketchPlane3d properties) =
    properties.yDirection
