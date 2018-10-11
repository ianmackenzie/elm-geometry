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


originPoint : Frame2d globalCoordinates localCoordinates -> Point2d globalCoordinates
originPoint (Frame2d properties) =
    properties.originPoint


xDirection : Frame2d globalCoordinates localCoordinates -> Direction2d globalCoordinates
xDirection (Frame2d properties) =
    properties.xDirection


yDirection : Frame2d globalCoordinates localCoordinates -> Direction2d globalCoordinates
yDirection (Frame2d properties) =
    properties.yDirection
