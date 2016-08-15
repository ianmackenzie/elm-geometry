{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module All exposing (..)

import Test
import Test.Runner.Html as Html
import Axis2d
import Axis3d
import Direction2d
import Direction3d
import Frame2d
import Frame3d
import Plane3d
import Point2d
import Point3d
import Vector2d
import Vector3d


suite =
    Test.describe "OpenSolid.Core"
        [ Axis2d.suite
        , Axis3d.suite
        , Direction2d.suite
        , Direction3d.suite
        , Frame2d.suite
        , Frame3d.suite
        , Plane3d.suite
        , Point2d.suite
        , Point3d.suite
        , Vector2d.suite
        , Vector3d.suite
        ]


main : Program Never
main =
    Html.run suite
