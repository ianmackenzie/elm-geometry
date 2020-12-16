--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector4d exposing
    ( Vector4d
    , dot
    , from
    , half
    , interpolateFrom
    , length
    , minus
    , plus
    , reverse
    , scaleBy
    , twice
    , unsafe
    , unwrap
    , xyzw
    , zero
    )

import Float.Extra as Float
import Geometry.Types as Types exposing (Point4d)
import Quantity exposing (Product, Quantity(..))
import Quantity.Extra as Quantity


type alias Vector4d units coordinates =
    Types.Vector4d units coordinates


unsafe : { x : Float, y : Float, z : Float, w : Float } -> Vector4d units coordinates
unsafe givenComponents =
    Types.Vector4d givenComponents


unwrap : Vector4d units coordinates -> { x : Float, y : Float, z : Float, w : Float }
unwrap (Types.Vector4d givenComponents) =
    givenComponents


zero : Vector4d units coordinates
zero =
    Types.Vector4d
        { x = 0
        , y = 0
        , z = 0
        , w = 0
        }


xyzw :
    Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Vector4d units coordinates
xyzw (Quantity x) (Quantity y) (Quantity z) (Quantity w) =
    Types.Vector4d
        { x = x
        , y = y
        , z = z
        , w = w
        }


from : Point4d units coordinates -> Point4d units coordinates -> Vector4d units coordinates
from (Types.Point4d p1) (Types.Point4d p2) =
    Types.Vector4d
        { x = p2.x - p1.x
        , y = p2.y - p1.y
        , z = p2.z - p1.z
        , w = p2.w - p1.w
        }


interpolateFrom : Vector4d units coordinates -> Vector4d units coordinates -> Float -> Vector4d units coordinates
interpolateFrom (Types.Vector4d v1) (Types.Vector4d v2) t =
    if t <= 0.5 then
        Types.Vector4d
            { x = v1.x + t * (v2.x - v1.x)
            , y = v1.y + t * (v2.y - v1.y)
            , z = v1.z + t * (v2.z - v1.z)
            , w = v1.w + t * (v2.w - v1.w)
            }

    else
        Types.Vector4d
            { x = v2.x + (1 - t) * (v1.x - v2.x)
            , y = v2.y + (1 - t) * (v1.y - v2.y)
            , z = v2.z + (1 - t) * (v1.z - v2.z)
            , w = v2.w + (1 - t) * (v1.w - v2.w)
            }


length : Vector4d units coordinates -> Quantity Float units
length (Types.Vector4d v) =
    let
        largestComponent =
            max (abs v.x) (max (abs v.y) (max (abs v.z) (abs v.w)))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledZ =
                v.z / largestComponent

            scaledW =
                v.w / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ + scaledW * scaledW)
        in
        Quantity (scaledLength * largestComponent)


plus : Vector4d units coordinates -> Vector4d units coordinates -> Vector4d units coordinates
plus (Types.Vector4d v2) (Types.Vector4d v1) =
    Types.Vector4d
        { x = v1.x + v2.x
        , y = v1.y + v2.y
        , z = v1.z + v2.z
        , w = v1.w + v2.w
        }


minus : Vector4d units coordinates -> Vector4d units coordinates -> Vector4d units coordinates
minus (Types.Vector4d v2) (Types.Vector4d v1) =
    Types.Vector4d
        { x = v1.x - v2.x
        , y = v1.y - v2.y
        , z = v1.z - v2.z
        , w = v1.w - v2.w
        }


dot : Vector4d units2 coordinates -> Vector4d units1 coordinates -> Quantity Float (Product units1 units2)
dot (Types.Vector4d v2) (Types.Vector4d v1) =
    Quantity (v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w)


twice : Vector4d units coordinates -> Vector4d units coordinates
twice vector =
    scaleBy 2 vector


half : Vector4d units coordinates -> Vector4d units coordinates
half vector =
    scaleBy 0.5 vector


reverse : Vector4d units coordinates -> Vector4d units coordinates
reverse (Types.Vector4d v) =
    Types.Vector4d
        { x = -v.x
        , y = -v.y
        , z = -v.z
        , w = -v.w
        }


scaleBy : Float -> Vector4d units coordinates -> Vector4d units coordinates
scaleBy k (Types.Vector4d v) =
    Types.Vector4d
        { x = k * v.x
        , y = k * v.y
        , z = k * v.z
        , w = k * v.w
        }
