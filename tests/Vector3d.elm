{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Vector3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Decode as Decode
import OpenSolid.Fuzz.Vector3d as Fuzz
import OpenSolid.Expect as Expect
import OpenSolid.Expect.Vector3d as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector3d Encode.vector3d Decode.vector3d


suite : Test
suite =
    Test.describe "OpenSolid.Core.Vector3d"
        [ jsonRoundTrips
        ]


main : Program Never
main =
    Html.run suite
