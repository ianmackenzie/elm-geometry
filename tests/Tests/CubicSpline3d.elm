module Tests.CubicSpline3d
    exposing
        ( arcLengthMatchesAnalytical
        , derivativeAndMagnitudeAreConsistent
        , fromEndpointsReproducesSpline
        , jsonRoundTrips
        , pointAtArcLengthIsEnd
        , pointAtZeroLengthIsStart
        )

import CubicSpline3d
import Expect exposing (FloatingPointTolerance(Absolute))
import Fuzz
import Geometry.Accuracy as Accuracy
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Tests.Generic as Generic
import Tests.QuadraticSpline3d
import Vector3d


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.cubicSpline3d
        Encode.cubicSpline3d
        Decode.cubicSpline3d


fromEndpointsReproducesSpline : Test
fromEndpointsReproducesSpline =
    Test.fuzz Fuzz.cubicSpline3d
        "CubicSpline3d.fromEndpoints reproduces original spline"
        (\spline ->
            let
                startPoint =
                    CubicSpline3d.startPoint spline

                endPoint =
                    CubicSpline3d.endPoint spline

                startDerivative =
                    CubicSpline3d.startDerivative spline

                endDerivative =
                    CubicSpline3d.endDerivative spline
            in
            CubicSpline3d.fromEndpoints
                { startPoint = startPoint
                , startDerivative = startDerivative
                , endPoint = endPoint
                , endDerivative = endDerivative
                }
                |> Expect.cubicSpline3d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz Tests.QuadraticSpline3d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline3d.fromQuadraticSpline
                |> CubicSpline3d.arcLengthParameterized
                    (Accuracy.maxError 1.0e-3)
                |> CubicSpline3d.arcLength
                |> Expect.within (Absolute 1.0e-3)
                    (Tests.QuadraticSpline3d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline3d
        "point along spline at zero length is start point"
        (\spline ->
            let
                parameterizedCurve =
                    CubicSpline3d.arcLengthParameterized
                        (Accuracy.maxError 1.0e-3)
                        spline
            in
            CubicSpline3d.pointAlong parameterizedCurve 0
                |> Expect.equal (Just (CubicSpline3d.startPoint spline))
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline3d
        "point along spline at arc length is end point"
        (\spline ->
            let
                parameterizedCurve =
                    CubicSpline3d.arcLengthParameterized
                        (Accuracy.maxError 1.0e-3)
                        spline

                arcLength =
                    CubicSpline3d.arcLength parameterizedCurve
            in
            CubicSpline3d.pointAlong parameterizedCurve arcLength
                |> Expect.equal (Just (CubicSpline3d.endPoint spline))
        )


derivativeAndMagnitudeAreConsistent : Test
derivativeAndMagnitudeAreConsistent =
    Test.fuzz2
        Fuzz.cubicSpline3d
        (Fuzz.floatRange 0 1)
        "derivative and derivativeMagnitude are consistent"
        (\spline t ->
            CubicSpline3d.derivative spline t
                |> Maybe.map Vector3d.length
                |> Expect.just Expect.approximately
                    (CubicSpline3d.derivativeMagnitude spline t)
        )
