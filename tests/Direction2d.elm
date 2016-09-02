{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Direction2d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Fuzz.Direction2d as Fuzz
import OpenSolid.Expect as Expect
import OpenSolid.Expect.Direction2d as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.direction2d
        Direction2d.encode
        Direction2d.decoder


suite : Test
suite =
    Test.describe "OpenSolid.Core.Direction2d"
        [ jsonRoundTrips
        ]


main : Program Never
main =
    Html.run suite
