module OpenSolid.Bootstrap.Plane3d
    exposing
        ( normalDirection
        , originPoint
        )

import OpenSolid.Geometry.Internal exposing (..)


originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection
