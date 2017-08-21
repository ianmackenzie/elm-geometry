module OpenSolid.Bootstrap.Axis3d
    exposing
        ( direction
        , originPoint
        , with
        )

import OpenSolid.Geometry.Internal exposing (..)


with : { originPoint : Point3d, direction : Direction3d } -> Axis3d
with =
    Axis3d


originPoint : Axis3d -> Point3d
originPoint (Axis3d properties) =
    properties.originPoint


direction : Axis3d -> Direction3d
direction (Axis3d properties) =
    properties.direction
