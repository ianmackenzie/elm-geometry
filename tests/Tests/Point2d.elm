module Tests.Point2d exposing
    ( circumcenterIsValidOrNothing
    , interpolationReturnsExactEndpoints
    , midpointIsEquidistant
    , projectionOntoAxisPreservesDistance
    , rotationPreservesDistance
    , translateByAndInAreConsistent
    , trickyCircumcenter
    )

import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Quantity exposing (zero)
import Test exposing (Test)
import Triangle2d
import Vector2d


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
    Test.fuzz3 Fuzz.point2d Fuzz.point2d Fuzz.angle description expectation


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


translateByAndInAreConsistent : Test
translateByAndInAreConsistent =
    Test.fuzz3
        Fuzz.point2d
        Fuzz.direction2d
        Fuzz.length
        "translateBy and translateIn are consistent"
        (\point direction distance ->
            let
                displacement =
                    Vector2d.withLength distance direction
            in
            point
                |> Point2d.translateIn direction distance
                |> Expect.point2d (Point2d.translateBy displacement point)
        )


circumcenterIsValidOrNothing : Test
circumcenterIsValidOrNothing =
    Test.fuzz3
        Fuzz.point2d
        Fuzz.point2d
        Fuzz.point2d
        "The circumcenter of three points is either Nothing or is equidistant from each point"
        (\p1 p2 p3 ->
            case Point2d.circumcenter p1 p2 p3 of
                Nothing ->
                    Triangle2d.area (Triangle2d.fromVertices p1 p2 p3)
                        |> Expect.approximately zero

                Just p0 ->
                    let
                        r1 =
                            p0 |> Point2d.distanceFrom p1
                    in
                    p0
                        |> Expect.all
                            [ Point2d.distanceFrom p2 >> Expect.approximately r1
                            , Point2d.distanceFrom p3 >> Expect.approximately r1
                            ]
        )


trickyCircumcenter : Test
trickyCircumcenter =
    Test.test "Circumcenter works correctly on previous failure case" <|
        \() ->
            let
                p1 =
                    Point2d.meters -10 0

                p2 =
                    Point2d.meters -10 1.0e-6

                p3 =
                    Point2d.meters -9.858773586876941 4.859985890767644

                p0 =
                    Point2d.meters 73.69327796224587 5.0e-7
            in
            Point2d.circumcenter p1 p2 p3 |> Expect.just Expect.point2d p0
