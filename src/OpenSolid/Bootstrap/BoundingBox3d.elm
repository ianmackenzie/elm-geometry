module OpenSolid.Bootstrap.BoundingBox3d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float } -> BoundingBox3d
with =
    BoundingBox3d
