--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Examples.Sphere3d exposing (exampleSphere)

import Point3d
import Sphere3d exposing (Sphere3d)


exampleSphere : Sphere3d
exampleSphere =
    Sphere3d.withRadius 3
        (Point3d.fromCoordinates ( 1, 2, 1 ))
