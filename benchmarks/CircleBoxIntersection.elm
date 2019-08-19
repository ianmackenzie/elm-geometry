module CircleBoxIntersection exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import Point2d


intersectsBoundingBoxAllocation : BoundingBox2d -> Circle2d -> Bool
intersectsBoundingBoxAllocation box circle =
    let
        ( boxMinX, boxMaxX ) =
            ( BoundingBox2d.minX box, BoundingBox2d.maxX box )

        ( boxMinY, boxMaxY ) =
            ( BoundingBox2d.minY box, BoundingBox2d.maxY box )

        circleRadius =
            Circle2d.radius circle

        ( circleX, circleY ) =
            Point2d.coordinates (Circle2d.centerPoint circle)

        deltaX =
            circleX - max boxMinX (min circleX boxMaxX)

        deltaY =
            circleY - max boxMinY (min circleY boxMaxY)
    in
    deltaX ^ 2 + deltaY ^ 2 <= circleRadius ^ 2


intersectsBoundingBoxNoAllocation : BoundingBox2d -> Circle2d -> Bool
intersectsBoundingBoxNoAllocation box circle =
    let
        boxMinX =
            BoundingBox2d.minX box

        boxMaxX =
            BoundingBox2d.maxX box

        boxMinY =
            BoundingBox2d.minY box

        boxMaxY =
            BoundingBox2d.maxY box

        circleRadius =
            Circle2d.radius circle

        ( circleX, circleY ) =
            Point2d.coordinates (Circle2d.centerPoint circle)

        deltaX =
            circleX - max boxMinX (min circleX boxMaxX)

        deltaY =
            circleY - max boxMinY (min circleY boxMaxY)
    in
    deltaX ^ 2 + deltaY ^ 2 <= circleRadius ^ 2


testBox : BoundingBox2d
testBox =
    BoundingBox2d.fromExtrema
        { minX = 1
        , minY = 1
        , maxX = 5
        , maxY = 4
        }


testCircle : Circle2d
testCircle =
    Circle2d.withRadius 3 (Point2d.fromCoordinates ( 6, 6 ))


suite : Benchmark
suite =
    Benchmark.describe "Circle/box intersection testing"
        [ Benchmark.benchmark "Allocation"
            (\() -> intersectsBoundingBoxAllocation testBox testCircle)
        , Benchmark.benchmark "No allocation"
            (\() -> intersectsBoundingBoxNoAllocation testBox testCircle)
        ]


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite
