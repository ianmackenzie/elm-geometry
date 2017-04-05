module OpenSolid.Bootstrap.SketchPlane3d
    exposing
        ( originPoint
        , xDirection
        , yDirection
        )

import OpenSolid.Geometry.Types exposing (..)


originPoint : SketchPlane3d -> Point3d
originPoint (SketchPlane3d properties) =
    properties.originPoint


xDirection : SketchPlane3d -> Direction3d
xDirection (SketchPlane3d properties) =
    properties.xDirection


yDirection : SketchPlane3d -> Direction3d
yDirection (SketchPlane3d properties) =
    properties.yDirection
