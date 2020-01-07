module CoordinateAccess exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Unitless)
import Random


testPoints : List (Point2d Unitless coordinates)
testPoints =
    let
        pointGenerator =
            Random.map2 Point2d.unitless
                (Random.float 0 100)
                (Random.float 0 100)

        listGenerator =
            Random.list 200 pointGenerator
    in
    Random.step listGenerator (Random.initialSeed 1)
        |> Tuple.first


centroid : Point2d units coordinates -> List (Point2d units coordinates) -> Point2d units coordinates
centroid (Types.Point2d p0) rest =
    centroidHelp p0.x p0.y 1 0 0 rest


centroidHelp : Float -> Float -> Float -> Float -> Float -> List (Point2d units coordinates) -> Point2d units coordinates
centroidHelp x0 y0 count dx dy points =
    case points of
        (Types.Point2d p) :: remaining ->
            centroidHelp
                x0
                y0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                remaining

        [] ->
            Types.Point2d
                { x = x0 + dx / count
                , y = y0 + dy / count
                }


centroid1 :
    Point2d Unitless coordinates
    -> List (Point2d Unitless coordinates)
    -> Point2d Unitless coordinates
centroid1 p0 rest =
    let
        (Quantity x0) =
            Point2d.xCoordinate p0

        (Quantity y0) =
            Point2d.yCoordinate p0
    in
    centroidHelp1 x0 y0 1 0 0 rest


centroidHelp1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List (Point2d Unitless coordinates)
    -> Point2d Unitless coordinates
centroidHelp1 x0 y0 count dx dy points =
    case points of
        point :: remaining ->
            let
                (Quantity x) =
                    Point2d.xCoordinate point

                (Quantity y) =
                    Point2d.yCoordinate point
            in
            centroidHelp1
                x0
                y0
                (count + 1)
                (dx + (x - x0))
                (dy + (y - y0))
                remaining

        [] ->
            Point2d.unitless
                (x0 + dx / count)
                (y0 + dy / count)


centroid2 :
    Point2d Unitless coordinates
    -> List (Point2d Unitless coordinates)
    -> Point2d Unitless coordinates
centroid2 p0 rest =
    let
        ( Quantity x0, Quantity y0 ) =
            Point2d.coordinates p0
    in
    centroidHelp2 x0 y0 1 0 0 rest


centroidHelp2 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List (Point2d Unitless coordinates)
    -> Point2d Unitless coordinates
centroidHelp2 x0 y0 count dx dy points =
    case points of
        point :: remaining ->
            let
                ( Quantity x, Quantity y ) =
                    Point2d.coordinates point
            in
            centroidHelp2
                x0
                y0
                (count + 1)
                (dx + (x - x0))
                (dy + (y - y0))
                remaining

        [] ->
            Point2d.unitless
                (x0 + dx / count)
                (y0 + dy / count)


suite : Benchmark
suite =
    Benchmark.compare "Coordinate access"
        -- "Default"
        -- (\() -> centroid Point2d.origin testPoints)
        "Individual"
        (\() -> centroid1 Point2d.origin testPoints)
        "Tuple"
        (\() -> centroid2 Point2d.origin testPoints)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite
