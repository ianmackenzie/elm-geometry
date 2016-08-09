{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Vector3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import ElmTest exposing (Test, test, assert)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Comparison exposing (valueIsOne)
import OpenSolid.Core.Test.Producer exposing (vector3d)


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.vector3d >> decodeValue Decode.vector3d)
        `is` Ok
        `for` vector3d


recordConversionRoundTrips : Claim
recordConversionRoundTrips =
    claim "Record conversion round-trips properly"
        `that` (Vector3d.toRecord >> Vector3d.fromRecord)
        `is` identity
        `for` vector3d


suite : Test
suite =
    ElmTest.suite "Vector3d tests"
        [ evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck recordConversionRoundTrips)
        ]


main =
    ElmTest.runSuiteHtml suite
