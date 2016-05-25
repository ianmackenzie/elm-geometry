{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Tests.Point2d exposing (suite)

import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import Check.Producer as Producer
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import TestUtils exposing (isApproximatelyZero)
import Producers exposing (angle, vector2d, point2d, axis2d)


rotationPreservesDistance : Claim
rotationPreservesDistance =
    let
        distancesAreEqual ( point, centerPoint, rotationAngle ) =
            let
                distance =
                    Point2d.distanceTo point centerPoint

                rotatedPoint =
                    Point2d.rotateAbout centerPoint rotationAngle point

                rotatedDistance =
                    Point2d.distanceTo rotatedPoint centerPoint
            in
                isApproximatelyZero (rotatedDistance - distance)
    in
        claim "Rotating about a point preserves distance from that point"
            `true` distancesAreEqual
            `for` Producer.tuple3 ( point2d, point2d, angle )


projectionOntoAxisPreservesDistance : Claim
projectionOntoAxisPreservesDistance =
    let
        distancesAreEqual ( point, axis ) =
            let
                distance =
                    Point2d.distanceAlong axis point

                projectedPoint =
                    Point2d.projectOnto axis point

                projectedDistance =
                    Point2d.distanceAlong axis projectedPoint
            in
                isApproximatelyZero (projectedDistance - distance)
    in
        claim "Projection onto axis preserves distance along that axis"
            `true` distancesAreEqual
            `for` Producer.tuple ( point2d, axis2d )


plusAndAddToAreEquivalent : Claim
plusAndAddToAreEquivalent =
    claim "Point2d.plus is equivalent to Vector2d.addTo"
        `that` uncurry Point2d.plus
        `is` uncurry (flip Vector2d.addTo)
        `for` Producer.tuple ( vector2d, point2d )


suite : Test
suite =
    ElmTest.suite "Point2d tests"
        [ evidenceToTest (quickCheck rotationPreservesDistance)
        , evidenceToTest (quickCheck projectionOntoAxisPreservesDistance)
        , evidenceToTest (quickCheck plusAndAddToAreEquivalent)
        ]
