module PerformanceTesting exposing (..)

import CubicSpline3d
import Test
import Test.Runner.Html exposing (TestProgram, run)


main : TestProgram
main =
    run <|
        Test.describe "CubicSpline3d"
            [ CubicSpline3d.hermiteReproducesSpline
            , CubicSpline3d.derivativeMagnitudeBoundsWorks
            , CubicSpline3d.arcLengthMatchesAnalytical
            , CubicSpline3d.pointAtZeroLengthIsStart
            , CubicSpline3d.pointAtArcLengthIsEnd
            ]
