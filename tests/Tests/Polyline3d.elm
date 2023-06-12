module Tests.Polyline3d exposing
    ( centroidIsWithinBoundingBox
    , centroidOfClosedSquare
    , centroidOfOpenSquare
    , centroidOfRightAngle
    , centroidOfSingleSegmentIsSameAsMidpoint
    , centroidOfStepShape
    , emptyPolylineHasNothingCentroid
    , zeroLengthPolylineHasItselfAsCentroid
    )

import BoundingBox3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (meters)
import Point3d
import Polyline3d
import Random
import Test exposing (Test)
import Test.Random as Test


emptyPolylineHasNothingCentroid : Test
emptyPolylineHasNothingCentroid =
    Test.test "Centroid is Nothing if Polyline is empty" <|
        \() ->
            let
                emptyPolyline =
                    Polyline3d.fromVertices []
            in
            Polyline3d.centroid emptyPolyline
                |> Expect.equal Nothing


zeroLengthPolylineHasItselfAsCentroid : Test
zeroLengthPolylineHasItselfAsCentroid =
    Test.check2 "Centroid of zero length polyline is the same point" Random.point3d (Random.int 1 20) <|
        \point reps ->
            let
                singlePointLine =
                    List.repeat reps point
                        |> Polyline3d.fromVertices
            in
            Polyline3d.centroid singlePointLine
                |> Expect.equal (Just point)


centroidOfExamplePolyline : Test
centroidOfExamplePolyline =
    Test.test "Example polyline from function docs" <|
        \() ->
            let
                examplePolyline =
                    Polyline3d.fromVertices
                        [ Point3d.fromTuple meters ( 0, 0, 0 )
                        , Point3d.fromTuple meters ( 1, 0, 0 )
                        , Point3d.fromTuple meters ( 1, 2, 0 )
                        , Point3d.fromTuple meters ( 1, 2, 3 )
                        ]
            in
            Polyline3d.centroid examplePolyline
                |> Expect.equal (Just (Point3d.fromTuple meters ( 5 / 6, 3 / 2, 0 )))


centroidOfSingleSegmentIsSameAsMidpoint : Test
centroidOfSingleSegmentIsSameAsMidpoint =
    Test.check2 "Centroid of single line segment is middle of endpoints" Random.point3d Random.point3d <|
        \p1 p2 ->
            let
                monoline =
                    Polyline3d.fromVertices [ p1, p2 ]

                expectedCentroid =
                    Point3d.midpoint p1 p2
            in
            Polyline3d.centroid monoline
                |> Expect.just (Expect.point3d expectedCentroid)


centroidOfRightAngle : Test
centroidOfRightAngle =
    Test.check "Centroid of a right angle is between the two sides" (Random.float -10 10) <|
        \armLength ->
            let
                angle =
                    Polyline3d.fromVertices
                        [ Point3d.fromTuple meters ( 0, 0, 0 )
                        , Point3d.fromTuple meters ( armLength, 0, 0 )
                        , Point3d.fromTuple meters ( armLength, 0, armLength )
                        ]

                expectedCentroid =
                    Point3d.meters (0.75 * armLength) 0 (0.25 * armLength)
            in
            Polyline3d.centroid angle
                |> Expect.just (Expect.point3d expectedCentroid)


centroidOfStepShape : Test
centroidOfStepShape =
    Test.check "Centroid of a step shape is halfway up the step" (Random.float -10 10) <|
        \armLength ->
            let
                angle =
                    Polyline3d.fromVertices
                        [ Point3d.fromTuple meters ( 0, 0, 0 )
                        , Point3d.fromTuple meters ( armLength, 0, 0 )
                        , Point3d.fromTuple meters ( armLength, armLength, 0 )
                        , Point3d.fromTuple meters ( 2 * armLength, armLength, 0 )
                        ]

                expectedCentroid =
                    Point3d.meters armLength (armLength / 2) 0
            in
            Polyline3d.centroid angle
                |> Expect.just (Expect.point3d expectedCentroid)


centroidOfOpenSquare : Test
centroidOfOpenSquare =
    Test.check "Centroid of an open square is skewed to closed side" (Random.float -10 10) <|
        \sideLength ->
            let
                squareline =
                    Polyline3d.fromVertices
                        [ Point3d.fromTuple meters ( 0, 0, 0 )
                        , Point3d.fromTuple meters ( 0, sideLength, 0 )
                        , Point3d.fromTuple meters ( sideLength, sideLength, 0 )
                        , Point3d.fromTuple meters ( sideLength, 0, 0 )
                        ]

                expectedCentroid =
                    Point3d.meters (sideLength / 2) (sideLength * 2 / 3) 0
            in
            Polyline3d.centroid squareline
                |> Expect.just (Expect.point3d expectedCentroid)


centroidOfClosedSquare : Test
centroidOfClosedSquare =
    Test.check "Centroid of a closed square is mid-point" (Random.float -10 10) <|
        \sideLength ->
            let
                squareline =
                    Polyline3d.fromVertices
                        [ Point3d.fromTuple meters ( 0, 0, 0 )
                        , Point3d.fromTuple meters ( 0, 0, sideLength )
                        , Point3d.fromTuple meters ( sideLength, 0, sideLength )
                        , Point3d.fromTuple meters ( sideLength, 0, 0 )
                        , Point3d.fromTuple meters ( 0, 0, 0 )
                        ]

                expectedCentroid =
                    Point3d.meters (sideLength / 2) 0 (sideLength / 2)
            in
            Polyline3d.centroid squareline
                |> Expect.just (Expect.point3d expectedCentroid)


centroidIsWithinBoundingBox : Test
centroidIsWithinBoundingBox =
    Test.check3 "The centroid of a polyline is within the polyline's bounding box"
        Random.point3d
        Random.point3d
        (Random.smallList Random.point3d)
        (\first second rest ->
            let
                points =
                    first :: second :: rest

                polyline =
                    Polyline3d.fromVertices points

                maybeBoundingBox =
                    Polyline3d.boundingBox polyline

                maybeCentroid =
                    Polyline3d.centroid polyline
            in
            case ( maybeBoundingBox, maybeCentroid ) of
                ( Just boundingBox, Just centroid ) ->
                    Expect.point3dContainedIn boundingBox centroid

                ( Nothing, _ ) ->
                    Expect.fail "Error determining bounding box."

                ( _, Nothing ) ->
                    Expect.fail "Error determining centroid."
        )
