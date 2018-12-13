--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Unsafe.Direction3d exposing (unsafeCrossProduct)

import Geometry.Types exposing (..)


unsafeCrossProduct : Direction3d coordinates -> Direction3d coordinates -> Direction3d coordinates
unsafeCrossProduct (Direction3d ( x1, y1, z1 )) (Direction3d ( x2, y2, z2 )) =
    Direction3d
        ( y1 * z2 - z1 * y2
        , z1 * x2 - x1 * z2
        , x1 * y2 - y1 * x2
        )
