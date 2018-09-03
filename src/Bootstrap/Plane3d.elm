--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Plane3d exposing
    ( normalDirection
    , originPoint
    )

import Geometry.Types exposing (..)


originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection
