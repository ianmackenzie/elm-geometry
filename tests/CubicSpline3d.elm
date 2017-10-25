module CubicSpline3d
    exposing
        ( arcLengthMatchesAnalytical
        , derivativeAndMagnitudeAreConsistent
        , hermiteReproducesSpline
        , jsonRoundTrips
        , pointAtArcLengthIsEnd
        , pointAtZeroLengthIsStart
        )

import Expect exposing (FloatingPointTolerance(Absolute))
import Fuzz
import Generic
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Vector3d as Vector3d
import QuadraticSpline3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.cubicSpline3d
        Encode.cubicSpline3d
        Decode.cubicSpline3d


hermiteReproducesSpline : Test
hermiteReproducesSpline =
    Test.fuzz Fuzz.cubicSpline3d
        "CubicSpline3d.hermite reproduces original spline"
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
            CubicSpline3d.hermite
                ( startPoint, startDerivative )
                ( endPoint, endDerivative )
                |> Expect.cubicSpline3d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz QuadraticSpline3d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline3d.fromQuadraticSpline
                |> CubicSpline3d.arcLengthParameterized 1.0e-3
                |> CubicSpline3d.arcLength
                |> Expect.within (Absolute 1.0e-3)
                    (QuadraticSpline3d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline3d
        "point along spline at zero length is start point"
        (\spline ->
            let
                parameterizedCurve =
                    CubicSpline3d.arcLengthParameterized 1.0e-3 spline
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
                    CubicSpline3d.arcLengthParameterized 1.0e-3 spline

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
                |> Vector3d.length
                |> Expect.approximately
                    (CubicSpline3d.derivativeMagnitude spline t)
        )
