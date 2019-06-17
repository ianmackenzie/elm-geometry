--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Point3d exposing
    ( xCoordinate
    , xyz
    , yCoordinate
    , zCoordinate
    )

import Geometry.Types exposing (..)
import Quantity exposing (Quantity)


xyz : Quantity Float units -> Quantity Float units -> Quantity Float units -> Point3d units coordinates
xyz x y z =
    Point3d ( x, y, z )


xCoordinate : Point3d units coordinates -> Quantity Float units
xCoordinate (Point3d ( x, y, z )) =
    x


yCoordinate : Point3d units coordinates -> Quantity Float units
yCoordinate (Point3d ( x, y, z )) =
    y


zCoordinate : Point3d units coordinates -> Quantity Float units
zCoordinate (Point3d ( x, y, z )) =
    z
