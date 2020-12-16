--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point4d exposing
    ( Point4d
    , interpolateFrom
    , midpoint
    , unsafe
    , unwrap
    , wCoordinate
    , xCoordinate
    , xyzw
    , yCoordinate
    , zCoordinate
    )

import Float.Extra as Float
import Geometry.Types as Types
import Quantity exposing (Quantity(..))
import Quantity.Extra as Quantity


{-| -}
type alias Point4d units coordinates =
    Types.Point4d units coordinates


unsafe : { x : Float, y : Float, z : Float, w : Float } -> Point4d units coordinates
unsafe givenCoordinates =
    Types.Point4d givenCoordinates


unwrap : Point4d units coordinates -> { x : Float, y : Float, z : Float, w : Float }
unwrap (Types.Point4d pointCoordinates) =
    pointCoordinates


xyzw :
    Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Point4d units coordinates
xyzw (Quantity x) (Quantity y) (Quantity z) (Quantity w) =
    Types.Point4d
        { x = x
        , y = y
        , z = z
        , w = w
        }


xCoordinate : Point4d units coordinates -> Quantity Float units
xCoordinate (Types.Point4d p) =
    Quantity p.x


yCoordinate : Point4d units coordinates -> Quantity Float units
yCoordinate (Types.Point4d p) =
    Quantity p.y


zCoordinate : Point4d units coordinates -> Quantity Float units
zCoordinate (Types.Point4d p) =
    Quantity p.z


wCoordinate : Point4d units coordinates -> Quantity Float units
wCoordinate (Types.Point4d p) =
    Quantity p.w


midpoint : Point4d units coordinates -> Point4d units coordinates -> Point4d units coordinates
midpoint (Types.Point4d p1) (Types.Point4d p2) =
    Types.Point4d
        { x = p1.x + 0.5 * (p2.x - p1.x)
        , y = p1.y + 0.5 * (p2.y - p1.y)
        , z = p1.z + 0.5 * (p2.z - p1.z)
        , w = p1.w + 0.5 * (p2.w - p1.w)
        }


interpolateFrom : Point4d units coordinates -> Point4d units coordinates -> Float -> Point4d units coordinates
interpolateFrom (Types.Point4d p1) (Types.Point4d p2) t =
    if t <= 0.5 then
        Types.Point4d
            { x = p1.x + t * (p2.x - p1.x)
            , y = p1.y + t * (p2.y - p1.y)
            , z = p1.z + t * (p2.z - p1.z)
            , w = p1.w + t * (p2.w - p1.w)
            }

    else
        Types.Point4d
            { x = p2.x + (1 - t) * (p1.x - p2.x)
            , y = p2.y + (1 - t) * (p1.y - p2.y)
            , z = p2.z + (1 - t) * (p1.z - p2.z)
            , w = p2.w + (1 - t) * (p1.w - p2.w)
            }
