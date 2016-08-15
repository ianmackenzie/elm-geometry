{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Vector3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector3d Encode.vector3d Decode.vector3d


recordConversionRoundTrips : Test
recordConversionRoundTrips =
    Generic.conversionRoundTrips "Record"
        Fuzz.vector3d
        Vector3d.toRecord
        Vector3d.fromRecord


suite : Test
suite =
    Test.describe "OpenSolid.Core.Vector3d"
        [ jsonRoundTrips
        , recordConversionRoundTrips
        ]


main : Program Never
main =
    Html.run suite
