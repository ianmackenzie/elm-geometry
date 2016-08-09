{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.All exposing (..)

import ElmTest
import OpenSolid.Core.Test.Axis2d as Axis2d
import OpenSolid.Core.Test.Axis3d as Axis3d
import OpenSolid.Core.Test.Direction2d as Direction2d
import OpenSolid.Core.Test.Direction3d as Direction3d
import OpenSolid.Core.Test.Frame2d as Frame2d
import OpenSolid.Core.Test.Frame3d as Frame3d
import OpenSolid.Core.Test.Plane3d as Plane3d
import OpenSolid.Core.Test.Point2d as Point2d
import OpenSolid.Core.Test.Point3d as Point3d
import OpenSolid.Core.Test.Vector2d as Vector2d
import OpenSolid.Core.Test.Vector3d as Vector3d


suite =
    ElmTest.suite "OpenSolid Core Tests"
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


main =
    ElmTest.runSuiteHtml suite
