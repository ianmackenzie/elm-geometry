--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Unsafe.Direction2d exposing (unsafeXyIn)

import Geometry.Types exposing (..)


unsafeXyIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Float -> Float -> Direction2d globalCoordinates
unsafeXyIn (Frame2d frame) x y =
    let
        (Direction2d i) =
            frame.xDirection

        (Direction2d j) =
            frame.yDirection
    in
    Direction2d
        { x = x * i.x + y * j.x
        , y = x * i.y + y * j.y
        }
