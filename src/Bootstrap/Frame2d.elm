--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Frame2d exposing
    ( originPoint
    , xDirection
    , yDirection
    )

import Geometry.Types exposing (..)


originPoint : Frame2d units coordinates1 coordinates2 -> Point2d units coordinates1
originPoint (Frame2d properties) =
    properties.originPoint


xDirection : Frame2d units coordinates1 coordinates2 -> Direction2d coordinates1
xDirection (Frame2d properties) =
    properties.xDirection


yDirection : Frame2d units coordinates1 coordinates2 -> Direction2d coordinates1
yDirection (Frame2d properties) =
    properties.yDirection
