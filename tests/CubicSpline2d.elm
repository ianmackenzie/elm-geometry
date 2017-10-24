module CubicSpline2d
    exposing
        ( arcLengthMatchesAnalytical
        , hermiteReproducesSpline
        , jsonRoundTrips
        , pointAtArcLengthIsEnd
        , pointAtZeroLengthIsStart
        )

import Expect exposing (FloatingPointTolerance(Absolute))
import Generic
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import QuadraticSpline2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.cubicSpline2d
        Encode.cubicSpline2d
        Decode.cubicSpline2d


hermiteReproducesSpline : Test
hermiteReproducesSpline =
    Test.fuzz Fuzz.cubicSpline2d
        "CubicSpline2d.hermite reproduces original spline"
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
            CubicSpline2d.hermite
                ( startPoint, startDerivative )
                ( endPoint, endDerivative )
                |> Expect.cubicSpline2d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz QuadraticSpline2d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline2d.fromQuadraticSpline
                |> CubicSpline2d.arcLengthParameterized 1.0e-3
                |> CubicSpline2d.arcLength
                |> Expect.within (Absolute 1.0e-3)
                    (QuadraticSpline2d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at zero length is start point"
        (\spline ->
            let
                parameterizedCurve =
                    CubicSpline2d.arcLengthParameterized 1.0e-3 spline
            in
            CubicSpline2d.pointAlong parameterizedCurve 0
                |> Expect.equal (Just (CubicSpline2d.startPoint spline))
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at arc length is end point"
        (\spline ->
            let
                parameterizedCurve =
                    CubicSpline2d.arcLengthParameterized 1.0e-3 spline

                arcLength =
                    CubicSpline2d.arcLength parameterizedCurve
            in
            CubicSpline2d.pointAlong parameterizedCurve arcLength
                |> Expect.equal (Just (CubicSpline2d.endPoint spline))
        )
