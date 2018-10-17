module Tests.Polyline3d exposing
    ( centroidIsWithinBoundingBox
    , centroidOfClosedSquare
    , centroidOfOpenSquare
    , centroidOfRightAngle
    , centroidOfSingleSegmentIsSameAsCentroidOfEndpoints
    , centroidOfStepShape
    , emptyPolylineHasNothingCentroid
    , zeroLengthPolylineHasItselfAsCentroid
    )

import BoundingBox3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point3d
import Polyline3d
import Test exposing (Test)


emptyPolylineHasNothingCentroid : Test
emptyPolylineHasNothingCentroid =
    Test.test "Centroid is Nothing if Polyline is empty" <|
        \_ ->
            let
                emptyPolyline =
                    Polyline3d.fromVertices []
            in
            Polyline3d.centroid emptyPolyline
                |> Expect.equal Nothing


zeroLengthPolylineHasItselfAsCentroid : Test
zeroLengthPolylineHasItselfAsCentroid =
    Test.fuzz2 Fuzz.point3d (Fuzz.intRange 1 20) "Centroid of zero length polyline is the same point" <|
        \point ->
            \reps ->
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
        \_ ->
            let
                examplePolyline =
                    Polyline3d.fromVertices
                        [ Point3d.fromCoordinates ( 0, 0, 0 )
                        , Point3d.fromCoordinates ( 1, 0, 0 )
                        , Point3d.fromCoordinates ( 1, 2, 0 )
                        , Point3d.fromCoordinates ( 1, 2, 3 )
                        ]
            in
            Polyline3d.centroid examplePolyline
                |> Expect.equal (Just (Point3d.fromCoordinates ( 5 / 6, 3 / 2, 0 )))


centroidOfSingleSegmentIsSameAsCentroidOfEndpoints : Test
centroidOfSingleSegmentIsSameAsCentroidOfEndpoints =
    Test.fuzz2 Fuzz.point3d Fuzz.point3d "Centroid of single line segment is middle of endpoints" <|
        \p1 ->
            \p2 ->
                let
                    monoline =
                        Polyline3d.fromVertices [ p1, p2 ]

                    ( x1, y1, z1 ) =
                        Point3d.coordinates p1

                    ( x2, y2, z2 ) =
                        Point3d.coordinates p2

                    expectedCentroid =
                        Point3d.fromCoordinates ( (x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2 )
                in
                Polyline3d.centroid monoline
                    |> Expect.just Expect.point3d expectedCentroid


centroidOfRightAngle : Test
centroidOfRightAngle =
    Test.fuzz Fuzz.float "Centroid of a right angle is between the two sides" <|
        \armLength ->
            let
                angle =
                    Polyline3d.fromVertices
                        [ Point3d.fromCoordinates ( 0, 0, 0 )
                        , Point3d.fromCoordinates ( armLength, 0, 0 )
                        , Point3d.fromCoordinates ( armLength, 0, armLength )
                        ]
            in
            Polyline3d.centroid angle
                |> Expect.just Expect.point3d (Point3d.fromCoordinates ( 0.75 * armLength, 0, 0.25 * armLength ))


centroidOfStepShape : Test
centroidOfStepShape =
    Test.fuzz Fuzz.float "Centroid of a step shape is halfway up the step" <|
        \armLength ->
            let
                angle =
                    Polyline3d.fromVertices
                        [ Point3d.fromCoordinates ( 0, 0, 0 )
                        , Point3d.fromCoordinates ( armLength, 0, 0 )
                        , Point3d.fromCoordinates ( armLength, armLength, 0 )
                        , Point3d.fromCoordinates ( 2 * armLength, armLength, 0 )
                        ]
            in
            Polyline3d.centroid angle
                |> Expect.just Expect.point3d (Point3d.fromCoordinates ( armLength, armLength / 2, 0 ))


centroidOfOpenSquare : Test
centroidOfOpenSquare =
    Test.fuzz Fuzz.float "Centroid of an open square is skewed to closed side" <|
        \sideLength ->
            let
                squareline =
                    Polyline3d.fromVertices
                        [ Point3d.fromCoordinates ( 0, 0, 0 )
                        , Point3d.fromCoordinates ( 0, sideLength, 0 )
                        , Point3d.fromCoordinates ( sideLength, sideLength, 0 )
                        , Point3d.fromCoordinates ( sideLength, 0, 0 )
                        ]
            in
            Polyline3d.centroid squareline
                |> Expect.just Expect.point3d (Point3d.fromCoordinates ( sideLength / 2, sideLength * 2 / 3, 0 ))


centroidOfClosedSquare : Test
centroidOfClosedSquare =
    Test.fuzz Fuzz.float "Centroid of a closed square is mid-point" <|
        \sideLength ->
            let
                squareline =
                    Polyline3d.fromVertices
                        [ Point3d.fromCoordinates ( 0, 0, 0 )
                        , Point3d.fromCoordinates ( 0, 0, sideLength )
                        , Point3d.fromCoordinates ( sideLength, 0, sideLength )
                        , Point3d.fromCoordinates ( sideLength, 0, 0 )
                        , Point3d.fromCoordinates ( 0, 0, 0 )
                        ]
            in
            Polyline3d.centroid squareline
                |> Expect.just Expect.point3d (Point3d.fromCoordinates ( sideLength / 2, 0, sideLength / 2 ))


centroidIsWithinBoundingBox : Test
centroidIsWithinBoundingBox =
    Test.fuzz3 Fuzz.point3d
        Fuzz.point3d
        (Fuzz.list Fuzz.point3d)
        "The centroid of a polyline is within the polyline's bounding box"
    <|
        \first ->
            \second ->
                \rest ->
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
                            Expect.boundingBox3dContains boundingBox centroid

                        ( Nothing, _ ) ->
                            Expect.fail "Error determining bounding box."

                        ( _, Nothing ) ->
                            Expect.fail "Error determining centroid."
