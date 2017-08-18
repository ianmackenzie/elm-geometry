module OpenSolid.Bootstrap.SketchPlane3d
    exposing
        ( originPoint
        , with
        , xDirection
        , yDirection
        )

import OpenSolid.Geometry.Types exposing (..)


with : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d } -> SketchPlane3d
with =
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
