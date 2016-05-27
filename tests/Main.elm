{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Main exposing (..)

import ElmTest
import Test.Axis2d
import Test.Axis3d
import Test.Direction2d
import Test.Direction3d
import Test.Frame2d
import Test.Frame3d
import Test.Plane3d
import Test.Point2d
import Test.Point3d
import Test.Vector2d
import Test.Vector3d


testSuites =
    [ Test.Axis2d.suite
    , Test.Axis3d.suite
    , Test.Direction2d.suite
    , Test.Direction3d.suite
    , Test.Frame2d.suite
    , Test.Frame3d.suite
    , Test.Plane3d.suite
    , Test.Point2d.suite
    , Test.Point3d.suite
    , Test.Vector2d.suite
    , Test.Vector3d.suite
    ]


main =
    ElmTest.runSuiteHtml (ElmTest.suite "OpenSolid Core Tests" testSuites)
