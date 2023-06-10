module Tests.Point3d exposing
    ( circumcenterIsValidOrNothing
    , coordinates
    , coordinatesIn
    , distanceFromIsCommutative
    , interpolationReturnsExactEndpoints
    , midpointIsEquidistant
    , mirrorFlipsSignedDistance
    , placeInIsInverseOfRelativeTo
    , projectIntoResultsInPerpendicularVector
    , projectIntoThenPlaceOntoIsProjectOnto
    , relativeToIsInverseOfPlaceIn
    , rotationAboutAxisPreservesDistanceAlong
    , rotationAboutAxisPreservesDistanceFrom
    , samePointsSameCentroid
    , translateByAndInAreConsistent
    , translationByPerpendicularDoesNotChangeSignedDistance
    )

import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Plane3d
import Point3d
import Quantity exposing (zero)
import Random
import SketchPlane3d
import Test exposing (Test)
import Test.Check as Test
import Triangle3d
import Vector3d


rotationAboutAxisPreservesDistanceAlong : Test
rotationAboutAxisPreservesDistanceAlong =
    let
        description =
            "Rotation around an axis preserves distance along that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.signedDistanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.signedDistanceAlong axis rotatedPoint
            in
            Expect.quantity distance rotatedDistance
    in
    Test.check3 description Random.point3d Random.axis3d Random.angle expectation


rotationAboutAxisPreservesDistanceFrom : Test
rotationAboutAxisPreservesDistanceFrom =
    let
        description =
            "Rotation around an axis preserves distance from that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.distanceFromAxis axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.distanceFromAxis axis rotatedPoint
            in
            Expect.quantity distance rotatedDistance
    in
    Test.check3 description Random.point3d Random.axis3d Random.angle expectation


midpointIsEquidistant : Test
midpointIsEquidistant =
    Test.check2 "Midpoint of two points is equidistant from those points"
        Random.point3d
        Random.point3d
        (\p1 p2 ->
            let
                midpoint =
                    Point3d.midpoint p1 p2
            in
            Expect.quantity
                (Point3d.distanceFrom p1 midpoint)
                (Point3d.distanceFrom p2 midpoint)
        )


interpolationReturnsExactEndpoints : Test
interpolationReturnsExactEndpoints =
    Test.check "Interpolation returns exact start point for t=0 and exact end point for t=1"
        (Random.pair Random.point3d Random.point3d)
        (Expect.all
            [ \( p1, p2 ) -> Point3d.interpolateFrom p1 p2 0 |> Expect.equal p1
            , \( p1, p2 ) -> Point3d.interpolateFrom p1 p2 1 |> Expect.equal p2
            ]
        )


projectIntoThenPlaceOntoIsProjectOnto : Test
projectIntoThenPlaceOntoIsProjectOnto =
    Test.check2 "Point3d.projectInto followed by Point3d.on is equivalent to Point3d.projectOnto"
        Random.point3d
        Random.sketchPlane3d
        (\point sketchPlane ->
            let
                plane =
                    SketchPlane3d.toPlane sketchPlane
            in
            Point3d.projectInto sketchPlane point
                |> Point3d.on sketchPlane
                |> Expect.point3d (Point3d.projectOnto plane point)
        )


mirrorFlipsSignedDistance : Test
mirrorFlipsSignedDistance =
    Test.check2 "'mirrorAcross plane' changes the sign of the 'signedDistanceFrom plane'"
        Random.point3d
        Random.plane3d
        (\point plane ->
            let
                signedDistance =
                    Point3d.signedDistanceFrom plane point
            in
            Point3d.mirrorAcross plane point
                |> Point3d.signedDistanceFrom plane
                |> Expect.quantity (Quantity.negate signedDistance)
        )


translationByPerpendicularDoesNotChangeSignedDistance : Test
translationByPerpendicularDoesNotChangeSignedDistance =
    Test.check3 "Translating in a direction perpendicular to a plane's normal direction does not change signed distance from that plane"
        Random.point3d
        Random.plane3d
        Random.length
        (\point plane distance ->
            let
                originalSignedDistance =
                    Point3d.signedDistanceFrom plane point

                normalDirection =
                    Plane3d.normalDirection plane

                perpendicularDirection =
                    Direction3d.perpendicularTo normalDirection
            in
            point
                |> Point3d.translateIn perpendicularDirection distance
                |> Point3d.signedDistanceFrom plane
                |> Expect.quantity originalSignedDistance
        )


translateByAndInAreConsistent : Test
translateByAndInAreConsistent =
    Test.check3 "translateBy and translateIn are consistent"
        Random.point3d
        Random.direction3d
        Random.length
        (\point direction distance ->
            let
                displacement =
                    Vector3d.withLength distance direction
            in
            point
                |> Point3d.translateIn direction distance
                |> Expect.point3d (Point3d.translateBy displacement point)
        )


projectIntoResultsInPerpendicularVector : Test
projectIntoResultsInPerpendicularVector =
    Test.check2 "The vector from a point to that point projected into a sketch plane is perpendicular to both sketch plane basis directions"
        Random.point3d
        Random.sketchPlane3d
        (\point sketchPlane ->
            let
                point2d =
                    Point3d.projectInto sketchPlane point

                point3d =
                    Point3d.on sketchPlane point2d

                displacement =
                    Vector3d.from point3d point

                xDirection =
                    SketchPlane3d.xDirection sketchPlane

                yDirection =
                    SketchPlane3d.yDirection sketchPlane
            in
            displacement
                |> Expect.all
                    [ Vector3d.componentIn xDirection >> Expect.quantity zero
                    , Vector3d.componentIn yDirection >> Expect.quantity zero
                    ]
        )


circumcenterIsValidOrNothing : Test
circumcenterIsValidOrNothing =
    Test.check3 "The circumcenter of three points is either Nothing or is equidistant from each point"
        Random.point3d
        Random.point3d
        Random.point3d
        (\p1 p2 p3 ->
            case Point3d.circumcenter p1 p2 p3 of
                Nothing ->
                    Triangle3d.area (Triangle3d.from p1 p2 p3)
                        |> Expect.quantity zero

                Just p0 ->
                    case Plane3d.throughPoints p1 p2 p3 of
                        Just plane ->
                            let
                                r1 =
                                    p0 |> Point3d.distanceFrom p1
                            in
                            p0
                                |> Expect.all
                                    [ Point3d.distanceFrom p2
                                        >> Expect.quantity r1
                                    , Point3d.distanceFrom p3
                                        >> Expect.quantity r1
                                    , Point3d.signedDistanceFrom plane
                                        >> Expect.quantity zero
                                    ]

                        Nothing ->
                            Expect.fail "Three points have a circumcenter but no plane through them"
        )


relativeToIsInverseOfPlaceIn : Test
relativeToIsInverseOfPlaceIn =
    Test.check2 "relativeTo is inverse of placeIn"
        Random.point3d
        Random.frame3d
        (\point frame ->
            point
                |> Point3d.placeIn frame
                |> Point3d.relativeTo frame
                |> Expect.point3d point
        )


placeInIsInverseOfRelativeTo : Test
placeInIsInverseOfRelativeTo =
    Test.check2 "placeIn is inverse of relativeTo"
        Random.point3d
        Random.frame3d
        (\point frame ->
            point
                |> Point3d.relativeTo frame
                |> Point3d.placeIn frame
                |> Expect.point3d point
        )


distanceFromIsCommutative : Test
distanceFromIsCommutative =
    Test.check2 "distance from here to there is the same as there to here"
        Random.point3d
        Random.point3d
        (\here there ->
            Point3d.distanceFrom here there
                |> Expect.equal (Point3d.distanceFrom there here)
        )


samePointsSameCentroid : Test
samePointsSameCentroid =
    Test.check3 "3 Point3ds should produce same centroid calling centroid or centroid3"
        Random.point3d
        Random.point3d
        Random.point3d
        (\p1 p2 p3 ->
            Point3d.centroid p1 [ p2, p3 ]
                |> Expect.point3d (Point3d.centroid3 p1 p2 p3)
        )


first : ( a, a, a ) -> a
first ( x, _, _ ) =
    x


second : ( a, a, a ) -> a
second ( _, y, _ ) =
    y


third : ( a, a, a ) -> a
third ( _, _, z ) =
    z


coordinates : Test
coordinates =
    Test.check "coordinates and xCoordinate etc. are consistent" Random.point3d <|
        \point ->
            Expect.all
                [ first >> Expect.quantity (Point3d.xCoordinate point)
                , second >> Expect.quantity (Point3d.yCoordinate point)
                , third >> Expect.quantity (Point3d.zCoordinate point)
                ]
                (Point3d.coordinates point)


coordinatesIn : Test
coordinatesIn =
    Test.check2 "coordinatesIn and xCoordinateIn etc. are consistent"
        Random.point3d
        Random.frame3d
        (\point frame ->
            Expect.all
                [ first >> Expect.quantity (Point3d.xCoordinateIn frame point)
                , second >> Expect.quantity (Point3d.yCoordinateIn frame point)
                , third >> Expect.quantity (Point3d.zCoordinateIn frame point)
                ]
                (Point3d.coordinatesIn frame point)
        )
