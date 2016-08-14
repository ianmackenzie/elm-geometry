{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Point2d exposing (suite)

import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Compare as Compare
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect


rotationPreservesDistance : Test
rotationPreservesDistance =
    let
        description =
            "Rotating about a point preserves distance from that point"

        expectation point centerPoint rotationAngle =
            let
                initialDistance =
                    Point2d.distanceFrom centerPoint point

                rotatedPoint =
                    Point2d.rotateAround centerPoint rotationAngle point

                rotatedDistance =
                    Point2d.distanceFrom centerPoint rotatedPoint
            in
                Expect.approximately initialDistance rotatedDistance
    in
        Test.fuzz3 Fuzz.point2d Fuzz.point2d Fuzz.scalar description expectation


projectionOntoAxisPreservesDistance : Test
projectionOntoAxisPreservesDistance =
    let
        description =
            "Projection onto axis preserves distance along that axis"

        expectation point axis =
            let
                distance =
                    Point2d.signedDistanceAlong axis point

                projectedPoint =
                    Point2d.projectOnto axis point

                projectedDistance =
                    Point2d.signedDistanceAlong axis projectedPoint
            in
                Expect.approximately projectedDistance distance
    in
        Test.fuzz2 Fuzz.point2d Fuzz.axis2d description expectation


jsonRoundTrips : Test
jsonRoundTrips =
    Test.fuzz Fuzz.point2d
        "JSON conversion round-trips properly"
        (\point ->
            point
                |> Encode.point2d
                |> Decode.decodeValue Decode.point2d
                |> Expect.equal (Ok point)
        )


recordConversionRoundTrips : Test
recordConversionRoundTrips =
    Test.fuzz Fuzz.point2d
        "Record conversion round-trips properly"
        (\point ->
            point
                |> Point2d.toRecord
                |> Point2d.fromRecord
                |> Expect.equal point
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Point2d"
        [ rotationPreservesDistance
        , projectionOntoAxisPreservesDistance
        , jsonRoundTrips
        , recordConversionRoundTrips
        ]


main : Program Never
main =
    Html.run suite
