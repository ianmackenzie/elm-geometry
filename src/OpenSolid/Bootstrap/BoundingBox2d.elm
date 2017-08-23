module OpenSolid.Bootstrap.BoundingBox2d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> BoundingBox2d
with ({ minX, maxX, minY, maxY } as extrema) =
    if minX <= maxX && minY <= maxY then
        BoundingBox2d extrema
    else
        BoundingBox2d
            { minX = min minX maxX
            , maxX = max minX maxX
            , minY = min minY maxY
            , maxY = max minY maxY
            }
