module Tests.Polyline2d exposing
    ( centroidIsWithinBoundingBox
    , centroidOfClosedSquare
    , centroidOfOpenSquare
    , centroidOfRightAngle
    , centroidOfSingleSegmentIsSameAsMidpoint
    , centroidOfStepShape
    , emptyPolylineHasNothingCentroid
    , zeroLengthPolylineHasItselfAsCentroid
    )

import BoundingBox2d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (meters)
import Point2d
import Polyline2d
import Random
import Test exposing (Test)
import Test.Random as Test


emptyPolylineHasNothingCentroid : Test
emptyPolylineHasNothingCentroid =
    Test.test "Centroid is Nothing if Polyline is empty" <|
        \() ->
            let
                emptyPolyline =
                    Polyline2d.fromVertices []
            in
            Polyline2d.centroid emptyPolyline
                |> Expect.equal Nothing


zeroLengthPolylineHasItselfAsCentroid : Test
zeroLengthPolylineHasItselfAsCentroid =
    Test.check2 "Centroid of zero length polyline is the same point"
        Random.point2d
        (Random.int 1 20)
        (\point reps ->
            let
                singlePointLine =
                    List.repeat reps point
                        |> Polyline2d.fromVertices
            in
            Polyline2d.centroid singlePointLine
                |> Expect.equal (Just point)
        )


centroidOfSingleSegmentIsSameAsMidpoint : Test
centroidOfSingleSegmentIsSameAsMidpoint =
    Test.check2 "Centroid of single line segment is middle of endpoints"
        Random.point2d
        Random.point2d
        (\p1 p2 ->
            let
                monoline =
                    Polyline2d.fromVertices [ p1, p2 ]

                expectedCentroid =
                    Point2d.midpoint p1 p2
            in
            Polyline2d.centroid monoline
                |> Expect.just (Expect.point2d expectedCentroid)
        )


centroidOfRightAngle : Test
centroidOfRightAngle =
    Test.check "Centroid of a right angle is between the two sides" (Random.float -10 10) <|
        \armLength ->
            let
                angle =
                    Polyline2d.fromVertices
                        [ Point2d.fromTuple meters ( 0, 0 )
                        , Point2d.fromTuple meters ( armLength, 0 )
                        , Point2d.fromTuple meters ( armLength, armLength )
                        ]

                expectedCentroid =
                    Point2d.meters (0.75 * armLength) (0.25 * armLength)
            in
            Polyline2d.centroid angle
                |> Expect.just (Expect.point2d expectedCentroid)


centroidOfStepShape : Test
centroidOfStepShape =
    Test.check "Centroid of a step shape is halfway up the step" (Random.float -10 10) <|
        \armLength ->
            let
                angle =
                    Polyline2d.fromVertices
                        [ Point2d.fromTuple meters ( 0, 0 )
                        , Point2d.fromTuple meters ( armLength, 0 )
                        , Point2d.fromTuple meters ( armLength, armLength )
                        , Point2d.fromTuple meters ( 2 * armLength, armLength )
                        ]

                expectedCentroid =
                    Point2d.meters armLength (armLength / 2)
            in
            Polyline2d.centroid angle
                |> Expect.just (Expect.point2d expectedCentroid)


centroidOfOpenSquare : Test
centroidOfOpenSquare =
    Test.check "Centroid of an open square is skewed to closed side" (Random.float -10 10) <|
        \sideLength ->
            let
                squareline =
                    Polyline2d.fromVertices
                        [ Point2d.fromTuple meters ( 0, 0 )
                        , Point2d.fromTuple meters ( 0, sideLength )
                        , Point2d.fromTuple meters ( sideLength, sideLength )
                        , Point2d.fromTuple meters ( sideLength, 0 )
                        ]

                expectedCentroid =
                    Point2d.meters (sideLength / 2) (sideLength * 2 / 3)
            in
            Polyline2d.centroid squareline
                |> Expect.just (Expect.point2d expectedCentroid)


centroidOfClosedSquare : Test
centroidOfClosedSquare =
    Test.check "Centroid of a closed square is mid-point" (Random.float -10 10) <|
        \sideLength ->
            let
                squareline =
                    Polyline2d.fromVertices
                        [ Point2d.fromTuple meters ( 0, 0 )
                        , Point2d.fromTuple meters ( 0, sideLength )
                        , Point2d.fromTuple meters ( sideLength, sideLength )
                        , Point2d.fromTuple meters ( sideLength, 0 )
                        , Point2d.fromTuple meters ( 0, 0 )
                        ]

                expectedCentroid =
                    Point2d.meters (sideLength / 2) (sideLength / 2)
            in
            Polyline2d.centroid squareline
                |> Expect.just (Expect.point2d expectedCentroid)


centroidIsWithinBoundingBox : Test
centroidIsWithinBoundingBox =
    Test.check3 "The centroid of a polyline is within the polyline's bounding box"
        Random.point2d
        Random.point2d
        (Random.smallList Random.point2d)
        (\first second rest ->
            let
                points =
                    first :: second :: rest

                polyline =
                    Polyline2d.fromVertices points

                maybeBoundingBox =
                    Polyline2d.boundingBox polyline

                maybeCentroid =
                    Polyline2d.centroid polyline
            in
            case ( maybeBoundingBox, maybeCentroid ) of
                ( Just boundingBox, Just centroid ) ->
                    Expect.point2dContainedIn boundingBox centroid

                ( Nothing, _ ) ->
                    Expect.fail "Error determining bounding box."

                ( _, Nothing ) ->
                    Expect.fail "Error determining centroid."
        )
