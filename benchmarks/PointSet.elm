module PointSet exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (small)
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Random
import Set2d exposing (Set2d)


type WorldCoordinates
    = WorldCoordinates


worldBounds : BoundingBox2d Meters WorldCoordinates
worldBounds =
    BoundingBox2d.from
        Point2d.origin
        (Point2d.meters 100 100)


smallSearchBox : BoundingBox2d Meters WorldCoordinates
smallSearchBox =
    BoundingBox2d.from (Point2d.meters 20 20) (Point2d.meters 30 30)


pointList : List (Point2d Meters WorldCoordinates)
pointList =
    Random.step (Random.list 1000 (BoundingBox2d.randomPoint worldBounds))
        (Random.initialSeed 1)
        |> Tuple.first


pointSet : Set2d Meters WorldCoordinates (Point2d Meters WorldCoordinates)
pointSet =
    Set2d.fromList BoundingBox2d.singleton pointList


pointInBox : BoundingBox2d Meters WorldCoordinates -> Point2d Meters WorldCoordinates -> Bool
pointInBox searchBox point =
    BoundingBox2d.contains point searchBox


suite : Benchmark
suite =
    Benchmark.describe "Point search"
        [ Benchmark.compare "Small search box"
            "List.filter"
            (\() -> List.filter (pointInBox smallSearchBox) pointList)
            "Set2d.search"
            (\() -> Set2d.search (Set2d.overlapping smallSearchBox) (always True) pointSet)
        , Benchmark.compare "Large search box"
            "List.filter"
            (\() -> List.filter (pointInBox worldBounds) pointList)
            "Set2d.search"
            (\() -> Set2d.search (Set2d.overlapping worldBounds) (always True) pointSet)
        ]


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite
