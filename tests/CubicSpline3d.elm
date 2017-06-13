module CubicSpline3d
    exposing
        ( hermiteReproducesSpline
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
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
