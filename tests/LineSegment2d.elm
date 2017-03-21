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


module LineSegment2d exposing (suite)

import Test exposing (Test)
import Expect
import Fuzz
import Test.Runner.Html as HtmlRunner
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.lineSegment2d
        Encode.lineSegment2d
        Decode.lineSegment2d


intersectionWorksProperly : Test
intersectionWorksProperly =
    let
        description =
            "Intersection of two line segments returns a point that is on both segments, if such a point exists"

        expectation firstSegment secondSegment =
            case LineSegment2d.intersectionPoint firstSegment secondSegment of
                Just point ->
                    let
                        -- Not enough by itself - point might be collinear with
                        -- but not actually on the segment (e.g. past the end)
                        isCollinearWith segment point =
                            let
                                ( startPoint, endPoint ) =
                                    LineSegment2d.endpoints segment

                                triangle =
                                    Triangle2d ( startPoint, endPoint, point )

                                area =
                                    Triangle2d.area triangle
                            in
                                Expect.approximately 0 area

                        -- Check that point is actually between the two
                        -- endpoints (almost enough of a check by itself, but
                        -- would not reliably detect small perpendicular
                        -- displacements from the segment)
                        isBetweenEndpointsOf segment point =
                            let
                                ( startPoint, endPoint ) =
                                    LineSegment2d.endpoints segment

                                firstDistance =
                                    Point2d.distanceFrom startPoint point

                                secondDistance =
                                    Point2d.distanceFrom point endPoint
                            in
                                Expect.approximately
                                    (LineSegment2d.length segment)
                                    (firstDistance + secondDistance)

                        isOn segment =
                            Expect.all
                                [ isCollinearWith segment
                                , isBetweenEndpointsOf segment
                                ]
                    in
                        Expect.all [ isOn firstSegment, isOn secondSegment ]
                            point

                Nothing ->
                    case
                        ( LineSegment2d.direction firstSegment
                        , LineSegment2d.direction secondSegment
                        )
                    of
                        ( Just firstDirection, Just secondDirection ) ->
                            let
                                firstStartPoint =
                                    LineSegment2d.startPoint firstSegment

                                secondStartPoint =
                                    LineSegment2d.startPoint secondSegment

                                firstAxis =
                                    Axis2d
                                        { originPoint = firstStartPoint
                                        , direction = firstDirection
                                        }

                                secondAxis =
                                    Axis2d
                                        { originPoint = secondStartPoint
                                        , direction = secondDirection
                                        }

                                oneOneSideOf axis segment =
                                    let
                                        ( startPoint, endPoint ) =
                                            LineSegment2d.endpoints segment

                                        startDistance =
                                            Point2d.signedDistanceFrom axis
                                                startPoint

                                        endDistance =
                                            Point2d.signedDistanceFrom axis
                                                endPoint

                                        tolerance =
                                            1.0e-12

                                        bothNonNegative =
                                            (startDistance > -tolerance)
                                                && (endDistance > -tolerance)

                                        bothNonPositive =
                                            (startDistance < tolerance)
                                                && (endDistance < tolerance)
                                    in
                                        bothNonNegative || bothNonPositive

                                -- Check if the first segment is fully on one
                                -- side of the second
                                firstBesideSecond =
                                    oneOneSideOf secondAxis firstSegment

                                -- Check if the second segment is fully on one
                                -- side of the first
                                secondBesideFirst =
                                    oneOneSideOf firstAxis secondSegment
                            in
                                Expect.true "One segment is fully one one side of the other"
                                    (secondBesideFirst || firstBesideSecond)

                        _ ->
                            Expect.pass
    in
        Test.fuzz2 Fuzz.lineSegment2d Fuzz.lineSegment2d description expectation


intersectionFindsCoincidentEndpoints : Test
intersectionFindsCoincidentEndpoints =
    let
        description =
            "Intersection of two segments with a shared endpoint returns that endpoint"

        expectation firstStart secondStart sharedEnd =
            let
                firstSegment =
                    LineSegment2d ( firstStart, sharedEnd )

                secondSegment =
                    LineSegment2d ( secondStart, sharedEnd )

                firstVector =
                    LineSegment2d.vector firstSegment

                secondVector =
                    LineSegment2d.vector secondSegment

                intersection =
                    LineSegment2d.intersectionPoint firstSegment secondSegment
            in
                if Vector2d.crossProduct firstVector secondVector /= 0 then
                    Expect.equal (Just sharedEnd) intersection
                else
                    Expect.pass
    in
        Test.fuzz3
            Fuzz.point2d
            Fuzz.point2d
            Fuzz.point2d
            description
            expectation


intersectionFindsCollinearCoincidentEndpoints : Test
intersectionFindsCollinearCoincidentEndpoints =
    let
        description =
            "Intersection of two collinear segments with one shared endpoint returns that endpoint"

        expectation startPoint vector t =
            let
                endPoint =
                    startPoint |> Point2d.translateBy vector

                midPoint =
                    Point2d.interpolateFrom startPoint endPoint t

                firstSegment =
                    LineSegment2d ( startPoint, midPoint )

                secondSegment =
                    LineSegment2d ( midPoint, endPoint )

                intersection1 =
                    LineSegment2d.intersectionPoint firstSegment secondSegment

                intersection2 =
                    LineSegment2d.intersectionPoint
                        firstSegment
                        (LineSegment2d.reverse secondSegment)

                intersection3 =
                    LineSegment2d.intersectionPoint
                        (LineSegment2d.reverse firstSegment)
                        secondSegment

                intersection4 =
                    LineSegment2d.intersectionPoint
                        (LineSegment2d.reverse firstSegment)
                        (LineSegment2d.reverse secondSegment)
            in
                Expect.all
                    [ Expect.equal intersection1
                    , Expect.equal intersection2
                    , Expect.equal intersection3
                    , Expect.equal intersection4
                    ]
                    (Just midPoint)
    in
        Test.fuzz3
            Fuzz.point2d
            Fuzz.vector2d
            (Fuzz.floatRange 0 1)
            description
            expectation


intersectionOfEqualPointSegmentIsPoint : Test
intersectionOfEqualPointSegmentIsPoint =
    Test.fuzz
        Fuzz.point2d
        "Intersection of trivial segment (a point) with itself is the point."
        (\point ->
            let
                segment =
                    LineSegment2d ( point, point )
            in
                LineSegment2d.intersectionPoint segment segment
                    |> Expect.equal (Just point)
        )


intersectionOfEqualLineSegmentsIsNothing : Test
intersectionOfEqualLineSegmentsIsNothing =
    Test.fuzz
        Fuzz.lineSegment2d
        "Intersection of two identical non degenerate line segments is Nothing"
        (\lineSegment ->
            let
                ( start, end ) =
                    LineSegment2d.endpoints lineSegment
            in
                if start == end then
                    Expect.pass
                else
                    LineSegment2d.intersectionPoint lineSegment lineSegment
                        |> Expect.equal Nothing
        )


intersectionOfReversedEqualLineSegmentsIsNothing : Test
intersectionOfReversedEqualLineSegmentsIsNothing =
    Test.fuzz
        Fuzz.lineSegment2d
        "Intersection of two reverse identical non degenerate line segments is Nothing"
        (\lineSegment ->
            let
                ( start, end ) =
                    LineSegment2d.endpoints lineSegment
            in
                if start == end then
                    Expect.pass
                else
                    LineSegment2d.reverse lineSegment
                        |> LineSegment2d.intersectionPoint lineSegment
                        |> Expect.equal Nothing
        )


sharedEndpointOnThirdSegmentInducesAnIntersection : Test
sharedEndpointOnThirdSegmentInducesAnIntersection =
    let
        description =
            "A shared endpoint on a third segment induces an intersection between the third segment and at least one of the other two segments sharing the endpoint."

        expectation segment3 point1 point2 =
            let
                sharedPoint =
                    LineSegment2d.midpoint segment3

                ( segment1, segment2 ) =
                    ( LineSegment2d ( sharedPoint, point1 )
                    , LineSegment2d ( sharedPoint, point2 )
                    )

                ( v1, v2, v3 ) =
                    ( LineSegment2d.vector segment1
                    , LineSegment2d.vector segment2
                    , LineSegment2d.vector segment3
                    )

                intersections =
                    ( LineSegment2d.intersectionPoint segment1 segment3
                    , LineSegment2d.intersectionPoint segment2 segment3
                    )

                ( v3Xv1, v3Xv2 ) =
                    ( Vector2d.crossProduct v3 v1
                    , Vector2d.crossProduct v3 v2
                    )
            in
                if v3Xv1 == 0 || v3Xv2 == 0 then
                    Expect.pass
                else if v3Xv1 * v3Xv2 > 0 then
                    -- point1 and point2 are on the same side of segment3
                    case intersections of
                        ( Nothing, Nothing ) ->
                            Expect.pass

                        ( Just p1, Just p2 ) ->
                            -- If intersection points are found for both
                            -- segments, they should be both be approximately
                            -- equal to the shared endpoint
                            sharedPoint
                                |> Expect.all
                                    [ Expect.point2d p1, Expect.point2d p2 ]

                        _ ->
                            Expect.fail "Behaviors different for segments on the same side"
                else
                    -- point1 and point2 are on opposite sides of segment3
                    case intersections of
                        ( Nothing, Nothing ) ->
                            Expect.fail "Shared endpoint intersection not found"

                        ( Just point, Nothing ) ->
                            -- If an intersection point is found for one
                            -- segment, it should be approximately equal to
                            -- the shared endpoint
                            point |> Expect.point2d sharedPoint

                        ( Nothing, Just point ) ->
                            -- If an intersection point is found for one
                            -- segment, it should be approximately equal to
                            -- the shared endpoint
                            point |> Expect.point2d sharedPoint

                        ( Just p1, Just p2 ) ->
                            -- If intersection points are found for both
                            -- segments, they should be both be approximately
                            -- equal to the shared endpoint
                            sharedPoint
                                |> Expect.all
                                    [ Expect.point2d p1, Expect.point2d p2 ]
    in
        Test.fuzz3
            Fuzz.lineSegment2d
            Fuzz.point2d
            Fuzz.point2d
            description
            expectation


intersectionIsSymmetric : Test
intersectionIsSymmetric =
    Test.fuzz2
        Fuzz.lineSegment2d
        Fuzz.lineSegment2d
        "Intersection should be symmetric"
        (\lineSegment1 lineSegment2 ->
            Expect.equal
                (LineSegment2d.intersectionPoint lineSegment1 lineSegment2)
                (LineSegment2d.intersectionPoint lineSegment2 lineSegment1)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.LineSegment2d"
        [ jsonRoundTrips
        , intersectionWorksProperly
        , intersectionFindsCoincidentEndpoints
        , intersectionFindsCollinearCoincidentEndpoints
        , intersectionOfEqualPointSegmentIsPoint
        , intersectionOfEqualLineSegmentsIsNothing
        , intersectionOfReversedEqualLineSegmentsIsNothing
        , sharedEndpointOnThirdSegmentInducesAnIntersection
        , intersectionIsSymmetric
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
