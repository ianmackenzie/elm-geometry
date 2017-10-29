module ArcLengthParameterization exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.CubicSpline3d as CubicSpline3d exposing (CubicSpline3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)


tolerance : Float
tolerance =
    1.0e-3


testQuadraticSpline2d : QuadraticSpline2d
testQuadraticSpline2d =
    QuadraticSpline2d.fromControlPoints
        ( Point2d.fromCoordinates ( 0, 0 )
        , Point2d.fromCoordinates ( 10, 10 )
        , Point2d.fromCoordinates ( 20, 0 )
        )


testQuadraticSpline3d : QuadraticSpline3d
testQuadraticSpline3d =
    QuadraticSpline3d.fromControlPoints
        ( Point3d.fromCoordinates ( 0, 0, 0 )
        , Point3d.fromCoordinates ( 10, 10, 0 )
        , Point3d.fromCoordinates ( 20, 0, 0 )
        )


testCubicSpline2d : CubicSpline2d
testCubicSpline2d =
    CubicSpline2d.fromControlPoints
        ( Point2d.fromCoordinates ( 0, 0 )
        , Point2d.fromCoordinates ( 10, 0 )
        , Point2d.fromCoordinates ( 0, 10 )
        , Point2d.fromCoordinates ( 10, 10 )
        )


testCubicSpline3d : CubicSpline3d
testCubicSpline3d =
    CubicSpline3d.fromControlPoints
        ( Point3d.fromCoordinates ( 0, 0, 0 )
        , Point3d.fromCoordinates ( 10, 0, 0 )
        , Point3d.fromCoordinates ( 10, 10, 0 )
        , Point3d.fromCoordinates ( 10, 10, 10 )
        )


suite : Benchmark
suite =
    Benchmark.describe "Arc length parameterization"
        [ Benchmark.benchmark2 "QuadraticSpline2d"
            QuadraticSpline2d.arcLengthParameterized
            tolerance
            testQuadraticSpline2d
        , Benchmark.benchmark2 "QuadraticSpline3d"
            QuadraticSpline3d.arcLengthParameterized
            tolerance
            testQuadraticSpline3d
        , Benchmark.benchmark2 "CubicSpline2d"
            CubicSpline2d.arcLengthParameterized
            tolerance
            testCubicSpline2d
        , Benchmark.benchmark2 "CubicSpline3d"
            CubicSpline3d.arcLengthParameterized
            tolerance
            testCubicSpline3d
        ]


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite
