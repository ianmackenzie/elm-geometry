--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Unsafe.Direction3d exposing (unsafeCrossProduct, unsafeXyOn)

import Geometry.Types exposing (..)


unsafeCrossProduct : Direction3d coordinates -> Direction3d coordinates -> Direction3d coordinates
unsafeCrossProduct (Direction3d d1) (Direction3d d2) =
    Direction3d
        { x = d1.y * d2.z - d1.z * d2.y
        , y = d1.z * d2.x - d1.x * d2.z
        , z = d1.x * d2.y - d1.y * d2.x
        }


unsafeXyOn : SketchPlane3d units coordinates { defines : localCoordinates } -> Float -> Float -> Direction3d coordinates
unsafeXyOn (SketchPlane3d sketchPlane) x y =
    let
        (Direction3d i) =
            sketchPlane.xDirection

        (Direction3d j) =
            sketchPlane.yDirection
    in
    Direction3d
        { x = x * i.x + y * j.x
        , y = x * i.y + y * j.y
        , z = x * i.z + y * j.z
        }
