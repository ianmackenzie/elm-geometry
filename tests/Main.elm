{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Main exposing (..)

import ElmTest exposing (..)
import Tests.Vector2d
import Tests.Vector3d
import Tests.Point2d


main =
    let
        tests =
            suite "OpenSolid Core Tests"
                [ Tests.Vector2d.suite
                , Tests.Vector3d.suite
                , Tests.Point2d.suite
                ]
    in
        runSuiteHtml tests
