module Tests.LineSegment3d exposing
    ( signedDistanceAlongContainsDistanceForAnyPoint
    , signedDistanceFromContainsDistanceForAnyPoint
    )

import Axis3d
import Expect
import Geometry.Random as Random
import LineSegment3d
import Point3d
import Quantity.Interval as Interval
import Test exposing (Test)
import Test.Check as Test


signedDistanceFromContainsDistanceForAnyPoint : Test
signedDistanceFromContainsDistanceForAnyPoint =
    Test.check3 "signedDistanceFrom contains distance for any point on the line segment"
        Random.lineSegment3d
        Random.parameterValue
        Random.plane3d
        (\lineSegment t plane ->
            let
                point =
                    LineSegment3d.interpolate lineSegment t
            in
            if
                LineSegment3d.signedDistanceFrom plane lineSegment
                    |> Interval.contains (Point3d.signedDistanceFrom plane point)
            then
                Expect.pass

            else
                Expect.fail "Interval should contain distance for any point on the line segment"
        )


signedDistanceAlongContainsDistanceForAnyPoint : Test
signedDistanceAlongContainsDistanceForAnyPoint =
    Test.check3 "signedDistanceAlong contains distance for any point on the line segment"
        Random.lineSegment3d
        Random.parameterValue
        Random.axis3d
        (\lineSegment t axis ->
            let
                point =
                    LineSegment3d.interpolate lineSegment t
            in
            if
                LineSegment3d.signedDistanceAlong axis lineSegment
                    |> Interval.contains (Point3d.signedDistanceAlong axis point)
            then
                Expect.pass

            else
                Expect.fail "Interval should contain distance for any point on the line segment"
        )
