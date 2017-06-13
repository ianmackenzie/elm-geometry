module CubicSpline2d
    exposing
        ( hermiteReproducesSpline
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
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
