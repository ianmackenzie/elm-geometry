{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Point3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import Check.Producer as Producer
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import Test.Utils exposing (valuesAreEqual)
import Test.Producers exposing (angle, vector3d, point3d, axis3d)


rotationAboutAxisPreservesDistance : Claim
rotationAboutAxisPreservesDistance =
    let
        distancesAreEqual ( point, axis, angle ) =
            let
                distance =
                    Point3d.distanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.distanceAlong axis rotatedPoint
            in
                valuesAreEqual distance rotatedDistance
    in
        claim "Rotation about axis preserves distance along that axis"
            `true` distancesAreEqual
            `for` Producer.tuple3 ( point3d, axis3d, angle )


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.point3d >> decodeValue Decode.point3d)
        `is` Ok
        `for` point3d


recordConversionRoundTrips : Claim
recordConversionRoundTrips =
    claim "Record conversion round-trips properly"
        `that` (Point3d.toRecord >> Point3d.fromRecord)
        `is` identity
        `for` point3d


suite : Test
suite =
    ElmTest.suite "Point3d tests"
        [ evidenceToTest (quickCheck rotationAboutAxisPreservesDistance)
        , evidenceToTest (quickCheck jsonRoundTrips)
        , evidenceToTest (quickCheck recordConversionRoundTrips)
        ]
