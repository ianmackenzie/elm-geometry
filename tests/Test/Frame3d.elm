{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Frame3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import Test.Utils exposing (valueIsOne)
import Test.Producers exposing (frame3d)


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.frame3d >> decodeValue Decode.frame3d)
        `is` Ok
        `for` frame3d


frameDirectionsAreOrthonormal : Claim
frameDirectionsAreOrthonormal =
    let
        directionsAreOrthonormal frame =
            let
                crossProduct =
                    Direction3d.cross frame.yDirection frame.xDirection

                tripleProduct =
                    Vector3d.componentIn frame.zDirection crossProduct
            in
                valueIsOne (Vector3d.length crossProduct)
                    && valueIsOne tripleProduct
    in
        claim "Frame3d basis directions are orthonormal"
            `true` directionsAreOrthonormal
            `for` frame3d


suite : Test
suite =
    ElmTest.suite "Frame3d tests"
        [ evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck frameDirectionsAreOrthonormal)
        ]
