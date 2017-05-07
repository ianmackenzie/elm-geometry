--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module CubicSpline3d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


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


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.CubicSpline3d"
        [ jsonRoundTrips
        , hermiteReproducesSpline
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
