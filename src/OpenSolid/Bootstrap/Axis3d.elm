module OpenSolid.Bootstrap.Axis3d
    exposing
        ( originPoint
        , direction
        )

import OpenSolid.Geometry.Types exposing (..)


originPoint : Axis3d -> Point3d
originPoint (Axis3d properties) =
    properties.originPoint


direction : Axis3d -> Direction3d
direction (Axis3d properties) =
    properties.direction
