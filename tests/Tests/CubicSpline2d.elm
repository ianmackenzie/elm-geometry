module Tests.CubicSpline2d exposing
    ( arcLengthMatchesAnalytical
    , fromEndpointsReproducesSpline
    , pointAtArcLengthIsEnd
    , pointAtZeroLengthIsStart
    )

import CubicSpline2d
import Expect exposing (FloatingPointTolerance(..))
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Length, inMeters, meters)
import Quantity exposing (zero)
import Test exposing (Test)
import Tests.QuadraticSpline2d


fromEndpointsReproducesSpline : Test
fromEndpointsReproducesSpline =
    Test.fuzz Fuzz.cubicSpline2d
        "CubicSpline2d.fromEndpoints reproduces original spline"
        (\spline ->
            let
                startPoint =
                    CubicSpline2d.startPoint spline

                endPoint =
                    CubicSpline2d.endPoint spline

                startDerivative =
                    CubicSpline2d.startDerivative spline

                endDerivative =
                    CubicSpline2d.endDerivative spline
            in
            CubicSpline2d.fromEndpoints startPoint startDerivative endPoint endDerivative
                |> Expect.cubicSpline2d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz Tests.QuadraticSpline2d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline2d.fromQuadraticSpline
                |> CubicSpline2d.nondegenerate
                |> Result.map
                    (CubicSpline2d.arcLengthParameterized
                        { maxError = meters 0.001 }
                        >> CubicSpline2d.arcLength
                    )
                |> Result.withDefault zero
                |> Expect.quantityWithin (meters 0.001)
                    (Tests.QuadraticSpline2d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at zero length is start point"
        (\spline ->
            case CubicSpline2d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }
                    in
                    CubicSpline2d.pointAlong parameterizedSpline (meters 0)
                        |> Expect.point2d (CubicSpline2d.startPoint spline)

                Err _ ->
                    Expect.pass
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at arc length is end point"
        (\spline ->
            case CubicSpline2d.nondegenerate spline of
                Ok nondegenerateSpline ->
                    let
                        parameterizedSpline =
                            nondegenerateSpline
                                |> CubicSpline2d.arcLengthParameterized
                                    { maxError = meters 1.0e-3 }

                        arcLength =
                            CubicSpline2d.arcLength parameterizedSpline
                    in
                    CubicSpline2d.pointAlong parameterizedSpline arcLength
                        |> Expect.point2d (CubicSpline2d.endPoint spline)

                Err _ ->
                    Expect.pass
        )
