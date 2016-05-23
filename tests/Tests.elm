{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Tests exposing (..)

import ElmTest exposing (..)
import VectorTests exposing (vectorTests)


main =
    let
        tests =
            suite "OpenSolid Core Tests" [ vectorTests ]
    in
        runSuiteHtml tests
