module Tests.LineSegment3d exposing
    ( signedDistanceAlongContainsDistanceForAnyPoint
    , signedDistanceFromContainsDistanceForAnyPoint
    )

import Axis3d
import Expect
import Fuzz
import Geometry.Fuzz as Fuzz
import LineSegment3d
import Point3d
import Quantity.Interval as Interval
import Test exposing (Test)


signedDistanceFromContainsDistanceForAnyPoint : Test
signedDistanceFromContainsDistanceForAnyPoint =
    Test.fuzz3
        Fuzz.lineSegment3d
        (Fuzz.floatRange 0 1)
        Fuzz.plane3d
        "signedDistanceFrom contains distance for any point on the line segment"
        (\lineSegment t plane ->
            let
                point =
                    LineSegment3d.interpolate lineSegment t
            in
            LineSegment3d.signedDistanceFrom plane lineSegment
                |> Interval.contains (Point3d.signedDistanceFrom plane point)
                |> Expect.true "Interval should contain distance for any point on the line segment"
        )


signedDistanceAlongContainsDistanceForAnyPoint : Test
signedDistanceAlongContainsDistanceForAnyPoint =
    Test.fuzz3
        Fuzz.lineSegment3d
        (Fuzz.floatRange 0 1)
        Fuzz.axis3d
        "signedDistanceAlong contains distance for any point on the line segment"
        (\lineSegment t axis ->
            let
                point =
                    LineSegment3d.interpolate lineSegment t
            in
            LineSegment3d.signedDistanceAlong axis lineSegment
                |> Interval.contains (Point3d.signedDistanceAlong axis point)
                |> Expect.true "Interval should contain distance for any point on the line segment"
        )
