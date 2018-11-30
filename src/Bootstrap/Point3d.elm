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
import Quantity exposing (Quantity)


fromCoordinates : ( Quantity Float units, Quantity Float units, Quantity Float units ) -> Point3d units coordinates
fromCoordinates givenCoordinates =
    Point3d givenCoordinates


coordinates : Point3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
coordinates (Point3d pointCoordinates) =
    pointCoordinates
