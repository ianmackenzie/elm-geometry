module Tests.VoronoiDiagram2d exposing
    ( cellForEveryInputVertex
    ,  failsOnCoincidentVertices
       --, pointInPolygonClosestToCorrespondingVertex

    )

import Array exposing (Array)
import BoundingBox2d
import DelaunayTriangulation2d
import Direction3d
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import List.Extra
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Polygon2d exposing (Polygon2d)
import Random
import Shrink
import SketchPlane3d
import Test exposing (Test)
import Vector3d
import VoronoiDiagram2d


uniquePoints : Fuzzer (Array Point2d)
uniquePoints =
    Fuzz.list Fuzz.point2d
        |> Fuzz.map (List.Extra.uniqueBy Point2d.coordinates)
        |> Fuzz.map Array.fromList


cellForEveryInputVertex : Test
cellForEveryInputVertex =
    let
        description =
            "A Voronoi diagram should include a cell for every input vertex (as long as the clipping bounding box contains all input vertices)"

        expectation points =
            case VoronoiDiagram2d.fromPoints points of
                Err _ ->
                    Expect.pass

                Ok diagram ->
                    case BoundingBox2d.containingPoints (Array.toList points) of
                        Nothing ->
                            let
                                boundingBox =
                                    BoundingBox2d.fromExtrema { minX = 0, minY = 0, maxX = 100, maxY = 100 }
                            in
                            VoronoiDiagram2d.polygons boundingBox diagram
                                |> Expect.equal []

                        Just boundingBox ->
                            diagram
                                |> VoronoiDiagram2d.polygons boundingBox
                                |> List.length
                                |> Expect.equal (Array.length points)
    in
    Test.fuzz uniquePoints description expectation


failsOnCoincidentVertices : Test
failsOnCoincidentVertices =
    let
        description =
            "Voronoi diagram construction should fail when coincident vertices are given"

        expectation points =
            case points of
                [] ->
                    Expect.pass

                x :: xs ->
                    let
                        pointsWithDuplicate =
                            Array.fromList (x :: x :: xs)
                    in
                    VoronoiDiagram2d.fromPoints pointsWithDuplicate
                        |> Expect.err
    in
    -- use normal `Fuzz.list`, more duplicates don't matter here
    Test.fuzz (Fuzz.list Fuzz.point2d) description expectation


pointInPolygon : Polygon2d -> Fuzzer Point2d
pointInPolygon polygon =
    let
        vertices =
            Polygon2d.vertices polygon

        numVertices =
            List.length vertices
    in
    case vertices of
        [] ->
            Fuzz.invalid "Can't generate a point inside an empty polygon"

        first :: rest ->
            let
                -- Ensuring every vertex has a positive weight avoids a divide
                -- by zero and ensures that the result is strictly inside the
                -- polygon
                weightsGenerator =
                    Random.list numVertices (Random.float 1 100)
            in
            Fuzz.custom weightsGenerator Shrink.noShrink
                |> Fuzz.map
                    (\weights ->
                        let
                            totalWeight =
                                List.sum weights

                            weightedXCoordinates =
                                List.map2
                                    (\weight vertex ->
                                        weight * Point2d.xCoordinate vertex
                                    )
                                    weights
                                    vertices

                            weightedYCoordinates =
                                List.map2
                                    (\weight vertex ->
                                        weight * Point2d.yCoordinate vertex
                                    )
                                    weights
                                    vertices

                            x =
                                List.sum weightedXCoordinates / totalWeight

                            y =
                                List.sum weightedYCoordinates / totalWeight
                        in
                        Point2d.fromCoordinates ( x, y )
                    )



--pointInPolygonClosestToCorrespondingVertex : Test
--pointInPolygonClosestToCorrespondingVertex =
--    let
--        description =
--            "Every point inside a Voronoi region must be closer to the corresponding vertex than any other vertex"
--        expectation points =
--            Expect.fail "TODO"
--    in
--    Test.fuzz uniquePoints description expectation
