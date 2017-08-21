module OpenSolid.Bootstrap.BoundingBox2d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> BoundingBox2d
with =
    BoundingBox2d
