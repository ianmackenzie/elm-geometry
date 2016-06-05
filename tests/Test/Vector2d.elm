{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Vector2d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import ElmTest exposing (Test, test, assert)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import Test.Utils exposing (valueIsOne, valueIsZero)
import Test.Producers exposing (vector2d)


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.vector2d >> decodeValue Decode.vector2d)
        `is` Ok
        `for` vector2d


recordConversionRoundTrips : Claim
recordConversionRoundTrips =
    claim "Record conversion round-trips properly"
        `that` (Vector2d.toRecord >> Vector2d.fromRecord)
        `is` identity
        `for` vector2d


perpendicularVectorIsPerpendicular : Claim
perpendicularVectorIsPerpendicular =
    let
        dotProductIsZero vector =
            let
                perpendicularVector =
                    Vector2d.perpendicularTo vector

                dotProduct =
                    Vector2d.dotProduct vector perpendicularVector
            in
                valueIsZero dotProduct
    in
        claim "perpendicularTo actually returns a perpendicular vector"
            `true` dotProductIsZero
            `for` vector2d


suite : Test
suite =
    ElmTest.suite "Vector2d tests"
        [ evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck recordConversionRoundTrips)
        , evidenceToTest (quickCheck perpendicularVectorIsPerpendicular)
        ]
