--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point2d exposing (suite)

import Test exposing (Test)
import Fuzz
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Point2d as Point2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


rotationPreservesDistance : Test
rotationPreservesDistance =
    let
        description =
            "Rotating around a point preserves distance from that point"

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
                    Point2d.distanceAlong axis point

                projectedPoint =
                    Point2d.projectOnto axis point

                projectedDistance =
                    Point2d.distanceAlong axis projectedPoint
            in
                Expect.approximately projectedDistance distance
    in
        Test.fuzz2 Fuzz.point2d Fuzz.axis2d description expectation


midpointIsEquidistant : Test
midpointIsEquidistant =
    Test.fuzz2
        Fuzz.point2d
        Fuzz.point2d
        "Midpoint of two points is equidistant from those points"
        (\p1 p2 ->
            let
                midpoint =
                    Point2d.midpoint p1 p2
            in
                Expect.approximately
                    (Point2d.distanceFrom p1 midpoint)
                    (Point2d.distanceFrom p2 midpoint)
        )


interpolationReturnsExactEndpoints : Test
interpolationReturnsExactEndpoints =
    Test.fuzz (Fuzz.tuple ( Fuzz.point2d, Fuzz.point2d ))
        "Interpolation returns exact start point for t=0 and exact end point for t=1"
        (Expect.all
            [ \( p1, p2 ) -> Point2d.interpolateFrom p1 p2 0 |> Expect.equal p1
            , \( p1, p2 ) -> Point2d.interpolateFrom p1 p2 1 |> Expect.equal p2
            ]
        )


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.point2d Encode.point2d Decode.point2d


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Point2d"
        [ rotationPreservesDistance
        , projectionOntoAxisPreservesDistance
        , midpointIsEquidistant
        , interpolationReturnsExactEndpoints
        , jsonRoundTrips
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
