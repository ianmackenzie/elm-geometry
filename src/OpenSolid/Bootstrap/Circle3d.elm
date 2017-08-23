module OpenSolid.Bootstrap.Circle3d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { centerPoint : Point3d, axialDirection : Direction3d, radius : Float } -> Circle3d
with properties =
    Circle3d { properties | radius = abs properties.radius }
