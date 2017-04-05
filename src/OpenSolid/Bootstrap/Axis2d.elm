module OpenSolid.Bootstrap.Axis2d
    exposing
        ( originPoint
        , direction
        )

import OpenSolid.Geometry.Types exposing (..)


originPoint : Axis2d -> Point2d
originPoint (Axis2d properties) =
    properties.originPoint


direction : Axis2d -> Direction2d
direction (Axis2d properties) =
    properties.direction
