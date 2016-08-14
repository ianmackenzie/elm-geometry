{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Vector3d exposing (suite)

import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Compare as Compare
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect


jsonRoundTrips : Test
jsonRoundTrips =
    Test.fuzz Fuzz.vector3d
        "JSON conversion round-trips properly"
        (\vector ->
            vector
                |> Encode.vector3d
                |> Decode.decodeValue Decode.vector3d
                |> Expect.equal (Ok vector)
        )


recordConversionRoundTrips : Test
recordConversionRoundTrips =
    Test.fuzz Fuzz.vector3d
        "Record conversion round-trips properly"
        (\vector ->
            vector
                |> Vector3d.toRecord
                |> Vector3d.fromRecord
                |> Expect.equal vector
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Vector3d"
        [ jsonRoundTrips
        , recordConversionRoundTrips
        ]


main : Program Never
main =
    Html.run suite
