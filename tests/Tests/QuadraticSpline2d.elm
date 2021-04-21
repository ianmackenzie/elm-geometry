module Tests.QuadraticSpline2d exposing
    ( analyticalLength
    , bSplineReproducesSpline
    , curvedSpline
    , degenerateSpline
    , exampleSpline
    , genericTests
    , line
    , parameterization
    )

import Angle
import ArcLengthParameterization
import Expect
import Float.Extra as Float
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Random as Random
import Length exposing (Length, Meters, inMeters, meters)
import Point2d
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity, zero)
import Test exposing (Test)
import Tests.Generic.Curve2d
import Tests.Literals exposing (ok)


curveOperations : Tests.Generic.Curve2d.Operations (QuadraticSpline2d Meters coordinates) coordinates
curveOperations =
    { generator = Random.quadraticSpline2d
    , pointOn = QuadraticSpline2d.pointOn
    , boundingBox = QuadraticSpline2d.boundingBox
    , firstDerivative = QuadraticSpline2d.firstDerivative
    , firstDerivativeBoundingBox = QuadraticSpline2d.firstDerivativeBoundingBox
    , scaleAbout = QuadraticSpline2d.scaleAbout
    , translateBy = QuadraticSpline2d.translateBy
    , rotateAround = QuadraticSpline2d.rotateAround
    , mirrorAcross = QuadraticSpline2d.mirrorAcross
    , numApproximationSegments = QuadraticSpline2d.numApproximationSegments
    }


degenerateSpline : Fuzzer (QuadraticSpline2d Meters coordinates)
degenerateSpline =
    Fuzz.point2d
        |> Fuzz.map
            (\point -> QuadraticSpline2d.fromControlPoints point point point)


curvedSpline : Fuzzer (QuadraticSpline2d Meters coordinates)
curvedSpline =
    Fuzz.map5
        (\angle length interpolationParameter offset flipSide ->
            let
                x0 =
                    Quantity.multiplyBy -0.5 length

                x2 =
                    Quantity.multiplyBy 0.5 length

                x1 =
                    Quantity.interpolateFrom x0 x2 interpolationParameter

                y1 =
                    if flipSide then
                        Quantity.negate offset

                    else
                        offset

                p0 =
                    Point2d.xy x0 zero

                p1 =
                    Point2d.xy x1 y1

                p2 =
                    Point2d.xy x2 zero
            in
            QuadraticSpline2d.fromControlPoints p0 p1 p2
                |> QuadraticSpline2d.rotateAround Point2d.origin angle
        )
        (Fuzz.map Angle.radians (Fuzz.floatRange 0 (2 * pi)))
        (Fuzz.map meters (Fuzz.floatRange 1 10))
        (Fuzz.floatRange 0 1)
        (Fuzz.map meters (Fuzz.floatRange 1 5))
        Fuzz.bool


analyticalLength : QuadraticSpline2d Meters coordinates -> Length
analyticalLength spline =
    let
        p0 =
            QuadraticSpline2d.firstControlPoint spline

        p1 =
            QuadraticSpline2d.secondControlPoint spline

        p2 =
            QuadraticSpline2d.thirdControlPoint spline

        ( x0, y0 ) =
            Point2d.toTuple inMeters p0

        ( x1, y1 ) =
            Point2d.toTuple inMeters p1

        ( x2, y2 ) =
            Point2d.toTuple inMeters p2

        ax =
            x0 - 2 * x1 + x2

        ay =
            y0 - 2 * y1 + y2

        bx =
            2 * x1 - 2 * x0

        by =
            2 * y1 - 2 * y0

        a =
            4 * (ax * ax + ay * ay)

        b =
            4 * (ax * bx + ay * by)

        c =
            bx * bx + by * by

        s_abc =
            2 * sqrt (a + b + c)

        a_2 =
            sqrt a

        a_32 =
            2 * a * a_2

        c_2 =
            2 * sqrt c

        ba =
            b / a_2
    in
    meters ((a_32 * s_abc + a_2 * b * (s_abc - c_2) + (4 * c * a - b * b) * logBase e ((2 * a_2 + ba + s_abc) / (ba + c_2))) / (4 * a_32))


exampleSpline : QuadraticSpline2d Meters coordinates
exampleSpline =
    QuadraticSpline2d.fromControlPoints
        (Point2d.fromTuple meters ( 1, 1 ))
        (Point2d.fromTuple meters ( 3, 4 ))
        (Point2d.fromTuple meters ( 5, 1 ))


parameterizedExample =
    QuadraticSpline2d.arcLengthParameterized { maxError = meters 0.001 }
        (ok (QuadraticSpline2d.nondegenerate exampleSpline))


line : QuadraticSpline2d Meters coordinates
line =
    QuadraticSpline2d.fromControlPoints
        (Point2d.fromTuple meters ( 0, 1 ))
        (Point2d.fromTuple meters ( 2.5, 1 ))
        (Point2d.fromTuple meters ( 5, 1 ))


parameterizedLine =
    QuadraticSpline2d.arcLengthParameterized { maxError = meters 0.001 }
        (ok (QuadraticSpline2d.nondegenerate line))


parameterization : Test
parameterization =
    Test.describe "arc length parameterization"
        [ Test.test "at s = 0 the result is the starting point of the spline" <|
            \_ ->
                QuadraticSpline2d.pointAlong parameterizedExample zero
                    |> Expect.point2d (Point2d.meters 1 1)
        , Test.fuzz Fuzz.quadraticSpline2d "at s = 0 the arcLengthParameterized curve gives the starting point" <|
            \spline ->
                case QuadraticSpline2d.nondegenerate spline of
                    Ok nondegenerateSpline ->
                        let
                            parameterizedSpline =
                                nondegenerateSpline
                                    |> QuadraticSpline2d.arcLengthParameterized
                                        { maxError = meters 0.001 }

                            startPoint =
                                QuadraticSpline2d.startPoint spline
                        in
                        QuadraticSpline2d.pointAlong parameterizedSpline zero
                            |> Expect.point2d startPoint

                    Err _ ->
                        Expect.pass
        , Test.fuzz Fuzz.quadraticSpline2d "at s = (length spline) the arcLengthParameterized curve gives the end point" <|
            \spline ->
                case QuadraticSpline2d.nondegenerate spline of
                    Ok nondegenerateSpline ->
                        let
                            parameterizedSpline =
                                nondegenerateSpline
                                    |> QuadraticSpline2d.arcLengthParameterized
                                        { maxError = meters 0.001 }

                            endPoint =
                                QuadraticSpline2d.endPoint spline
                        in
                        QuadraticSpline2d.arcLength parameterizedSpline
                            |> QuadraticSpline2d.pointAlong parameterizedSpline
                            |> Expect.point2d endPoint

                    Err _ ->
                        Expect.pass
        , Test.fuzz Fuzz.quadraticSpline2d "at s = (length spline) toParameterValue gives 1" <|
            \spline ->
                case QuadraticSpline2d.nondegenerate spline of
                    Ok nondegenerateSpline ->
                        let
                            parameterizedSpline =
                                nondegenerateSpline
                                    |> QuadraticSpline2d.arcLengthParameterized
                                        { maxError = meters 0.001 }

                            arcLength =
                                QuadraticSpline2d.arcLength parameterizedSpline

                            parameterValue =
                                parameterizedSpline
                                    |> QuadraticSpline2d.arcLengthParameterization
                                    |> ArcLengthParameterization.arcLengthToParameterValue arcLength
                        in
                        if arcLength == zero then
                            parameterValue |> Expect.exactly 0

                        else
                            parameterValue |> Expect.exactly 1

                    Err _ ->
                        Expect.pass
        , Test.fuzz curvedSpline "arc length matches analytical formula" <|
            -- analyticalLength falls down for degenerate splines so just check
            -- curved ones
            \spline ->
                let
                    tolerance =
                        meters 0.001

                    measuredArcLength =
                        spline
                            |> QuadraticSpline2d.nondegenerate
                            |> Result.map
                                (QuadraticSpline2d.arcLengthParameterized
                                    { maxError = tolerance }
                                    >> QuadraticSpline2d.arcLength
                                )
                            |> Result.withDefault zero
                in
                measuredArcLength
                    |> Expect.quantityWithin tolerance (analyticalLength spline)
        , Test.test "at s = 5 the result is the end point of the line" <|
            \_ ->
                QuadraticSpline2d.pointAlong parameterizedLine (meters 5)
                    |> Expect.point2d (Point2d.meters 5 1)
        , Test.test "at s = 2.5 the result is the midpoint of the line" <|
            \_ ->
                QuadraticSpline2d.pointAlong parameterizedLine (meters 2.5)
                    |> Expect.point2d (Point2d.meters 2.5 1)
        , Test.test "at s=2.5 is about right" <|
            \_ ->
                let
                    expected =
                        Point2d.meters 2.9008070813684963 2.4963102868350115
                in
                QuadraticSpline2d.pointAlong parameterizedExample (meters 2.5)
                    |> Expect.point2d expected
        , Test.test "length is as expected" <|
            \_ ->
                QuadraticSpline2d.arcLength parameterizedLine
                    |> Expect.quantity (meters 5)
        , Test.test "length parameterization at approximate length is Just" <|
            \_ ->
                let
                    arcLength =
                        QuadraticSpline2d.arcLength parameterizedExample
                in
                QuadraticSpline2d.pointAlong parameterizedExample arcLength
                    |> Expect.point2d (Point2d.meters 5 1)
        ]


bSplineReproducesSpline : Test
bSplineReproducesSpline =
    Test.fuzz Fuzz.quadraticSpline2d
        "Can reconstruct a quadratic spline with a B-spline with repeated knots"
        (\spline ->
            let
                p1 =
                    QuadraticSpline2d.firstControlPoint spline

                p2 =
                    QuadraticSpline2d.secondControlPoint spline

                p3 =
                    QuadraticSpline2d.thirdControlPoint spline

                bSplineSegments =
                    QuadraticSpline2d.bSplineSegments [ 0, 0, 1, 1 ]
                        [ p1, p2, p3 ]
            in
            case bSplineSegments of
                [ singleSegment ] ->
                    singleSegment |> Expect.quadraticSpline2d spline

                _ ->
                    Expect.fail "Expected a single B-spline segment"
        )


genericTests : Test
genericTests =
    Tests.Generic.Curve2d.tests
        curveOperations
        curveOperations
        QuadraticSpline2d.placeIn
        QuadraticSpline2d.relativeTo
