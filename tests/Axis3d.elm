{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Axis3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Fuzz.Axis3d as Fuzz
import OpenSolid.Expect as Expect
import OpenSolid.Expect.Axis3d as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.axis3d Axis3d.encode Axis3d.decoder


suite : Test
suite =
    Test.describe "OpenSolid.Core.Axis3d"
        [ jsonRoundTrips
        ]


main : Program Never
main =
    Html.run suite
