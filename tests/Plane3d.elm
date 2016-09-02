{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Plane3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Fuzz.Plane3d as Fuzz
import OpenSolid.Expect as Expect
import OpenSolid.Expect.Plane3d as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.plane3d Plane3d.encode Plane3d.decoder


suite : Test
suite =
    Test.describe "OpenSolid.Core.Plane3d"
        [ jsonRoundTrips
        ]


main : Program Never
main =
    Html.run suite
