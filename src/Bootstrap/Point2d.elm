--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Point2d exposing
    ( xCoordinate
    , xy
    , yCoordinate
    )

import Geometry.Types exposing (..)
import Quantity exposing (Quantity)


xy : Quantity Float units -> Quantity Float units -> Point2d units coordinates
xy x y =
    Point2d ( x, y )


xCoordinate : Point2d units coordinates -> Quantity Float units
xCoordinate (Point2d ( x, y )) =
    x


yCoordinate : Point2d units coordinates -> Quantity Float units
yCoordinate (Point2d ( x, y )) =
    y
