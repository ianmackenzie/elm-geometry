--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Point3d exposing
    ( coordinates
    , fromCoordinates
    )

import Geometry.Types exposing (..)


fromCoordinates : ( Float, Float, Float ) -> Point3d
fromCoordinates =
    Point3d


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_
