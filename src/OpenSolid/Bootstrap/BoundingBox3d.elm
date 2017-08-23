module OpenSolid.Bootstrap.BoundingBox3d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float } -> BoundingBox3d
with ({ minX, maxX, minY, maxY, minZ, maxZ } as extrema) =
    if minX <= maxX && minY <= maxY && minZ <= maxZ then
        BoundingBox3d extrema
    else
        BoundingBox3d
            { minX = min minX maxX
            , maxX = max minX maxX
            , minY = min minY maxY
            , maxY = max minY maxY
            , minZ = min minZ maxZ
            , maxZ = max minZ maxZ
            }
