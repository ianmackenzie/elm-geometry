module Tests.LineSegment2d exposing
    ( intersectionFindsCoincidentEndpoints
    , intersectionFindsCollinearCoincidentEndpoints
    , intersectionIsSymmetric
    , intersectionOfEqualLineSegmentsIsNothing
    , intersectionOfEqualPointSegmentIsPoint
    , intersectionOfReversedEqualLineSegmentsIsNothing
    , intersectionWorksProperly
    , reversingDoesNotAffectIntersection
    , sharedEndpointOnThirdSegmentInducesAnIntersection
    , signedDistanceAlongContainsDistanceForAnyPoint
    , signedDistanceFromContainsDistanceForAnyPoint
    )

import Axis2d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (meters)
import LineSegment2d
import Point2d
import Quantity
import Quantity.Interval as Interval
import Test exposing (Test)
import Test.Random as Test
import Triangle2d
import Vector2d


intersectionWorksProperly : Test
intersectionWorksProperly =
    let
        description =
            "Intersection of two line segments returns a point that is on both segments, if such a point exists"

        expectation firstSegment secondSegment =
            case LineSegment2d.intersectionPoint firstSegment secondSegment of
                Just intersectionPoint ->
                    let
                        -- Not enough by itself - point might be collinear with
                        -- but not actually on the segment (e.g. past the end)
                        isCollinearWith segment point =
                            let
                                ( startPoint, endPoint ) =
                                    LineSegment2d.endpoints segment

                                triangle =
                                    Triangle2d.from startPoint endPoint point

                                area =
                                    Triangle2d.area triangle
                            in
                            Expect.quantity Quantity.zero area

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
                            Expect.quantity
                                (LineSegment2d.length segment)
                                (firstDistance |> Quantity.plus secondDistance)

                        isOn segment =
                            Expect.all
                                [ isCollinearWith segment
                                , isBetweenEndpointsOf segment
                                ]
                    in
                    Expect.all [ isOn firstSegment, isOn secondSegment ]
                        intersectionPoint

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
                                    Axis2d.through firstStartPoint
                                        firstDirection

                                secondAxis =
                                    Axis2d.through secondStartPoint
                                        secondDirection

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
                                            meters 1.0e-12

                                        negativeTolerance =
                                            Quantity.negate tolerance

                                        bothNonNegative =
                                            (startDistance
                                                |> Quantity.greaterThan
                                                    negativeTolerance
                                            )
                                                && (endDistance
                                                        |> Quantity.greaterThan
                                                            negativeTolerance
                                                   )

                                        bothNonPositive =
                                            (startDistance
                                                |> Quantity.lessThan
                                                    tolerance
                                            )
                                                && (endDistance
                                                        |> Quantity.lessThan
                                                            tolerance
                                                   )
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
                            if secondBesideFirst || firstBesideSecond then
                                Expect.pass

                            else
                                Expect.fail "One segment is fully one one side of the other"

                        _ ->
                            Expect.pass
    in
    Test.check2 description Random.lineSegment2d Random.lineSegment2d expectation


intersectionFindsCoincidentEndpoints : Test
intersectionFindsCoincidentEndpoints =
    let
        description =
            "Intersection of two segments with a shared endpoint returns that endpoint"

        expectation firstStart secondStart sharedEnd =
            let
                firstSegment =
                    LineSegment2d.from firstStart sharedEnd

                secondSegment =
                    LineSegment2d.from secondStart sharedEnd

                firstVector =
                    LineSegment2d.vector firstSegment

                secondVector =
                    LineSegment2d.vector secondSegment

                intersection =
                    LineSegment2d.intersectionPoint firstSegment secondSegment
            in
            if
                (firstVector |> Vector2d.cross secondVector)
                    /= Quantity.zero
            then
                Expect.equal (Just sharedEnd) intersection

            else
                Expect.pass
    in
    Test.check3 description
        Random.point2d
        Random.point2d
        Random.point2d
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
                    LineSegment2d.from startPoint midPoint

                secondSegment =
                    LineSegment2d.from midPoint endPoint

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
    Test.check3 description
        Random.point2d
        Random.vector2d
        Random.parameterValue
        expectation


intersectionOfEqualPointSegmentIsPoint : Test
intersectionOfEqualPointSegmentIsPoint =
    Test.check "Intersection of trivial segment (a point) with itself is the point."
        Random.point2d
        (\point ->
            let
                segment =
                    LineSegment2d.fromEndpoints ( point, point )
            in
            LineSegment2d.intersectionPoint segment segment
                |> Expect.equal (Just point)
        )


intersectionOfEqualLineSegmentsIsNothing : Test
intersectionOfEqualLineSegmentsIsNothing =
    Test.check "Intersection of two identical non degenerate line segments is Nothing"
        Random.lineSegment2d
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
    Test.check "Intersection of two reverse identical non degenerate line segments is Nothing"
        Random.lineSegment2d
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
                    ( LineSegment2d.from sharedPoint point1
                    , LineSegment2d.from sharedPoint point2
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
                    ( v3 |> Vector2d.cross v1
                    , v3 |> Vector2d.cross v2
                    )
            in
            if v3Xv1 == Quantity.zero || v3Xv2 == Quantity.zero then
                Expect.pass

            else if
                (v3Xv1 |> Quantity.times v3Xv2)
                    |> Quantity.greaterThan Quantity.zero
            then
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

                    ( Just p1, Nothing ) ->
                        -- If an intersection point is found for only segment1,
                        -- then that point should be approximately equal to
                        -- sharedPoint and segment2 should be approximately
                        -- parallel to segment3
                        ( p1, v3Xv2 )
                            |> Expect.all
                                [ Tuple.first >> Expect.point2d sharedPoint
                                , Tuple.second
                                    >> Expect.quantity Quantity.zero
                                ]

                    ( Nothing, Just p2 ) ->
                        -- If an intersection point is found for only segment2,
                        -- then that point should be approximately equal to
                        -- sharedPoint and segment1 should be approximately
                        -- parallel to segment3
                        ( p2, v3Xv1 )
                            |> Expect.all
                                [ Tuple.first >> Expect.point2d sharedPoint
                                , Tuple.second
                                    >> Expect.quantity Quantity.zero
                                ]

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
    Test.check3 description
        Random.lineSegment2d
        Random.point2d
        Random.point2d
        expectation


intersectionIsSymmetric : Test
intersectionIsSymmetric =
    Test.check2 "Intersection should be (approximately) symmetric"
        Random.lineSegment2d
        Random.lineSegment2d
        (\lineSegment1 lineSegment2 ->
            let
                intersection12 =
                    LineSegment2d.intersectionPoint lineSegment1 lineSegment2

                intersection21 =
                    LineSegment2d.intersectionPoint lineSegment2 lineSegment1
            in
            case ( intersection12, intersection21 ) of
                ( Just p1, Just p2 ) ->
                    p1 |> Expect.point2d p2

                _ ->
                    Expect.equal intersection12 intersection21
        )


reversingDoesNotAffectIntersection : Test
reversingDoesNotAffectIntersection =
    Test.check2 "Reversing one line segment should not change the intersection point"
        Random.lineSegment2d
        Random.lineSegment2d
        (\lineSegment1 lineSegment2 ->
            let
                normalIntersection =
                    LineSegment2d.intersectionPoint lineSegment1 lineSegment2

                reversedIntersection =
                    LineSegment2d.intersectionPoint lineSegment1
                        (LineSegment2d.reverse lineSegment2)
            in
            case ( normalIntersection, reversedIntersection ) of
                ( Just p1, Just p2 ) ->
                    p1 |> Expect.point2d p2

                _ ->
                    Expect.equal normalIntersection reversedIntersection
        )


signedDistanceFromContainsDistanceForAnyPoint : Test
signedDistanceFromContainsDistanceForAnyPoint =
    Test.check3 "signedDistanceFrom contains distance for any point on the line segment"
        Random.lineSegment2d
        Random.parameterValue
        Random.axis2d
        (\lineSegment t axis ->
            let
                point =
                    LineSegment2d.interpolate lineSegment t
            in
            if
                LineSegment2d.signedDistanceFrom axis lineSegment
                    |> Interval.contains (Point2d.signedDistanceFrom axis point)
            then
                Expect.pass

            else
                Expect.fail "Interval should contain distance for any point on the line segment"
        )


signedDistanceAlongContainsDistanceForAnyPoint : Test
signedDistanceAlongContainsDistanceForAnyPoint =
    Test.check3
        "signedDistanceAlong contains distance for any point on the line segment"
        Random.lineSegment2d
        Random.parameterValue
        Random.axis2d
        (\lineSegment t axis ->
            let
                point =
                    LineSegment2d.interpolate lineSegment t
            in
            if
                LineSegment2d.signedDistanceAlong axis lineSegment
                    |> Interval.contains (Point2d.signedDistanceAlong axis point)
            then
                Expect.pass

            else
                Expect.fail "Interval should contain distance for any point on the line segment"
        )
