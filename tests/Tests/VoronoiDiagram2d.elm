module Tests.VoronoiDiagram2d exposing
    ( cellForEveryInputVertex
    , failsOnCoincidentVertices
    , pointInPolygonClosestToCorrespondingVertex
    )

import DelaunayTriangulation2d
import Direction3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d
import Point3d
import SketchPlane3d
import Test exposing (Test)
import Vector3d
import VoronoiDiagram2d


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
                    case BoundingBox2d.aggregate points of
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
                                |> Expect.equal (List.length points)
    in
    Test.fuzz3 (Fuzz.listWithoutDuplicates Fuzz.point2d) description expectation


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
                            x :: x :: xs
                    in
                    VoronoiDiagram2d.fromPoints pointsWithDuplicate
                        |> Expect.err
    in
    -- use normal `Fuzz.list`, more duplicates don't matter here
    Test.fuzz3 (Fuzz.list Fuzz.point2d) description expectation


pointInPolygonClosestToCorrespondingVertex : Test
pointInPolygonClosestToCorrespondingVertex =
    let
        description =
            "Every point inside a Voronoi region must be closer to the corresponding vertex than any other vertex"

        expectation points =
            Expect.fail "TODO"
    in
    Test.fuzz3 (Fuzz.listWithoutDuplicates Fuzz.point2d) description expectation
