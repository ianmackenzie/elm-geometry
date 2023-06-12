module Tests.Point2d exposing
    ( circumcenterIsValidOrNothing
    , consistentRecordInterop
    , consistentTupleInterop
    , coordinates
    , coordinatesIn
    , interpolationReturnsExactEndpoints
    , midpointIsEquidistant
    , projectionOntoAxisPreservesDistance
    , rotationPreservesDistance
    , translateByAndInAreConsistent
    , trickyCircumcenter
    )

import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length
import Point2d
import Quantity exposing (zero)
import Random
import Test exposing (Test)
import Test.Random as Test
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
            Expect.quantity initialDistance rotatedDistance
    in
    Test.check3 description Random.point2d Random.point2d Random.angle expectation


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
            Expect.quantity projectedDistance distance
    in
    Test.check2 description Random.point2d Random.axis2d expectation


midpointIsEquidistant : Test
midpointIsEquidistant =
    Test.check2 "Midpoint of two points is equidistant from those points"
        Random.point2d
        Random.point2d
        (\p1 p2 ->
            let
                midpoint =
                    Point2d.midpoint p1 p2
            in
            Expect.quantity
                (Point2d.distanceFrom p1 midpoint)
                (Point2d.distanceFrom p2 midpoint)
        )


interpolationReturnsExactEndpoints : Test
interpolationReturnsExactEndpoints =
    Test.check "Interpolation returns exact start point for t=0 and exact end point for t=1"
        (Random.pair Random.point2d Random.point2d)
        (Expect.all
            [ \( p1, p2 ) -> Point2d.interpolateFrom p1 p2 0 |> Expect.equal p1
            , \( p1, p2 ) -> Point2d.interpolateFrom p1 p2 1 |> Expect.equal p2
            ]
        )


translateByAndInAreConsistent : Test
translateByAndInAreConsistent =
    Test.check3 "translateBy and translateIn are consistent"
        Random.point2d
        Random.direction2d
        Random.length
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
    Test.check3 "The circumcenter of three points is either Nothing or is equidistant from each point"
        Random.point2d
        Random.point2d
        Random.point2d
        (\p1 p2 p3 ->
            case Point2d.circumcenter p1 p2 p3 of
                Nothing ->
                    Triangle2d.area (Triangle2d.from p1 p2 p3)
                        |> Expect.quantity zero

                Just p0 ->
                    let
                        r1 =
                            p0 |> Point2d.distanceFrom p1
                    in
                    p0
                        |> Expect.all
                            [ Point2d.distanceFrom p2 >> Expect.quantity r1
                            , Point2d.distanceFrom p3 >> Expect.quantity r1
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
            Point2d.circumcenter p1 p2 p3 |> Expect.just (Expect.point2d p0)


consistentTupleInterop : Test
consistentTupleInterop =
    Test.check "toTuple and then fromTuple return the same point" Random.point2d <|
        \point ->
            Point2d.toTuple Length.inMeters point
                |> Point2d.fromTuple Length.meters
                |> Expect.equal point


consistentRecordInterop : Test
consistentRecordInterop =
    Test.check "toRecord and then fromRecord return the same point" Random.point2d <|
        \point ->
            Point2d.toRecord Length.inInches point
                |> Point2d.fromRecord Length.inches
                |> Expect.point2d point


coordinates : Test
coordinates =
    Test.check "coordinates and xCoordinate/yCoordinate are consistent" Random.point2d <|
        \point ->
            Expect.all
                [ Tuple.first >> Expect.quantity (Point2d.xCoordinate point)
                , Tuple.second >> Expect.quantity (Point2d.yCoordinate point)
                ]
                (Point2d.coordinates point)


coordinatesIn : Test
coordinatesIn =
    Test.check2 "coordinatesIn and xCoordinateIn/yCoordinateIn are consistent"
        Random.point2d
        Random.frame2d
        (\point frame ->
            Expect.all
                [ Tuple.first >> Expect.quantity (Point2d.xCoordinateIn frame point)
                , Tuple.second >> Expect.quantity (Point2d.yCoordinateIn frame point)
                ]
                (Point2d.coordinatesIn frame point)
        )
