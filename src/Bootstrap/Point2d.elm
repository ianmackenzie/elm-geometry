--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Point2d exposing
    ( coordinates
    , fromCoordinates
    )

import Geometry.Types exposing (..)


fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates =
    Point2d


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates_) =
    coordinates_
