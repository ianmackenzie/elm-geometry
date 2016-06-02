{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Vector3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import ElmTest exposing (Test, test, assert)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import Test.Utils exposing (valueIsOne)
import Test.Producers exposing (vector3d)


normalizationWorksProperly : Claim
normalizationWorksProperly =
    let
        normalizeResultIsCorrect vector =
            case (Vector3d.normalize vector) of
                Nothing ->
                    -- If normalized result is Nothing, input must have been the
                    -- zero vector
                    vector == Vector3d.zero

                Just normalized ->
                    -- Otherwise, normalized length should be nearly 1
                    valueIsOne (Vector3d.length normalized)
    in
        claim "Normalization works properly"
            `true` normalizeResultIsCorrect
            `for` vector3d


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
        [ evidenceToTest (quickCheck normalizationWorksProperly)
        , evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck recordConversionRoundTrips)
        ]
