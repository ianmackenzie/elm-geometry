module OpenSolid.Bootstrap.Axis2d
    exposing
        ( direction
        , originPoint
        )

import OpenSolid.Geometry.Internal exposing (..)


originPoint : Axis2d -> Point2d
originPoint (Axis2d properties) =
    properties.originPoint


direction : Axis2d -> Direction2d
direction (Axis2d properties) =
    properties.direction
