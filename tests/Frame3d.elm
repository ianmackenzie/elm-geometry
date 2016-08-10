{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Frame3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Frame3d as Frame3d
import OpenSolid.Core.Test.Comparison exposing (valueIsOne)
import OpenSolid.Core.Test.Producer exposing (frame3d)


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
                    Direction3d.crossProduct (Frame3d.xDirection frame)
                        (Frame3d.yDirection frame)

                tripleProduct =
                    Vector3d.componentIn (Frame3d.zDirection frame) crossProduct
            in
                valueIsOne (Vector3d.length crossProduct)
                    && valueIsOne tripleProduct
    in
        claim "Frame3d basis directions are orthonormal"
            `true` directionsAreOrthonormal
            `for` frame3d


suite : Test
suite =
    ElmTest.suite "OpenSolid.Core.Frame3d"
        [ evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck frameDirectionsAreOrthonormal)
        ]


main =
    ElmTest.runSuiteHtml suite
