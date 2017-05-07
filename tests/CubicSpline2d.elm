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


module CubicSpline2d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


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


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.CubicSpline2d"
        [ jsonRoundTrips
        , hermiteReproducesSpline
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
