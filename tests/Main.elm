{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Main exposing (..)

import ElmTest
import Tests.Vector2d
import Tests.Vector3d
import Tests.Direction2d
import Tests.Direction3d
import Tests.Point2d
import Tests.Point3d


testSuites =
    [ Tests.Vector2d.suite
    , Tests.Vector3d.suite
    , Tests.Direction2d.suite
    , Tests.Direction3d.suite
    , Tests.Point2d.suite
    , Tests.Point3d.suite
    ]


main =
    ElmTest.runSuiteHtml (ElmTest.suite "OpenSolid Core Tests" testSuites)
