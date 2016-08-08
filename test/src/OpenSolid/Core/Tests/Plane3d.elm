{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Tests.Plane3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Producers exposing (plane3d)


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.plane3d >> decodeValue Decode.plane3d)
        `is` Ok
        `for` plane3d


suite : Test
suite =
    ElmTest.suite "Plane3d tests"
        [ evidenceToTest (quickCheck jsonRoundTrips)
        ]
