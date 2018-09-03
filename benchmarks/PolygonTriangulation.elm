module PolygonTriangulation exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import BoundingBox2d exposing (BoundingBox2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Random.Pcg as Random
import Vector2d exposing (Vector2d)


generator : Random.Generator Polygon2d
generator =
    let
        boundingBox =
            BoundingBox2d.fromExtrema
                { minX = 0
                , maxX = 100
                , minY = 0
                , maxY = 100
                }

        centerPoint =
            BoundingBox2d.centroid boundingBox

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        minRadius =
            0.05 * min width height

        maxRadius =
            0.5 * min width height - minRadius

        midRadius =
            (minRadius + maxRadius) / 2

        innerRadiusGenerator =
            Random.float minRadius (midRadius - 5)

        outerRadiusGenerator =
            Random.float (midRadius + 5) maxRadius

        numPoints =
            250
    in
    Random.list numPoints
        (Random.pair innerRadiusGenerator outerRadiusGenerator)
        |> Random.map
            (List.indexedMap
                (\index ( innerRadius, outerRadius ) ->
                    let
                        angle =
                            turns 1
                                * toFloat index
                                / toFloat numPoints

                        innerRadialVector =
                            Vector2d.fromPolarComponents
                                ( innerRadius
                                , angle
                                )

                        outerRadialVector =
                            Vector2d.fromPolarComponents
                                ( outerRadius
                                , angle
                                )

                        innerPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    innerRadialVector

                        outerPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    outerRadialVector
                    in
                    ( innerPoint, outerPoint )
                )
            )
        |> Random.map List.unzip
        |> Random.map
            (\( innerLoop, outerLoop ) ->
                Polygon2d.with
                    { outerLoop = outerLoop
                    , innerLoops = [ List.reverse innerLoop ]
                    }
            )


testPolygon : Polygon2d
testPolygon =
    let
        ( result, updatedSeed ) =
            Random.step generator (Random.initialSeed 3)
    in
    result


suite : Benchmark
suite =
    Benchmark.benchmark "Polygon triangulation"
        (\() -> Polygon2d.triangulate testPolygon)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite
