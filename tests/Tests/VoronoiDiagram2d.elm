module Tests.VoronoiDiagram2d exposing
    ( cellForEveryInputVertex
    , failsOnCoincidentVertices
    )

import Array exposing (Array)
import BoundingBox2d
import DelaunayTriangulation2d
import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters, inMeters, meters)
import List.Extra
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Polygon2d exposing (Polygon2d)
import Quantity
import Random exposing (Generator)
import SketchPlane3d
import Test exposing (Test)
import Test.Check as Test
import Vector3d
import VoronoiDiagram2d


uniquePoints : Generator (Array (Point2d Meters coordinates))
uniquePoints =
    Random.smallList Random.point2d
        |> Random.map (List.Extra.uniqueBy (Point2d.toTuple inMeters))
        |> Random.map Array.fromList


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
                    case BoundingBox2d.hullN (Array.toList points) of
                        Nothing ->
                            let
                                boundingBox =
                                    BoundingBox2d.fromExtrema
                                        { minX = meters 0
                                        , minY = meters 0
                                        , maxX = meters 100
                                        , maxY = meters 100
                                        }
                            in
                            VoronoiDiagram2d.polygons boundingBox diagram
                                |> Expect.equal []

                        Just boundingBox ->
                            diagram
                                |> VoronoiDiagram2d.polygons (BoundingBox2d.expandBy (Length.centimeters 1) boundingBox)
                                |> List.length
                                |> Expect.equal (Array.length points)
    in
    Test.check description uniquePoints expectation


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
    -- use normal `Random.smallList`, more duplicates don't matter here
    Test.check description (Random.smallList Random.point2d) expectation



-- pointInPolygon : Polygon2d Meters coordinates -> Fuzzer (Point2d Meters coordinates)
-- pointInPolygon polygon =
--     let
--         vertices =
--             Polygon2d.vertices polygon
--         numVertices =
--             List.length vertices
--     in
--     case vertices of
--         [] ->
--             Fuzz.invalid "Can't generate a point inside an empty polygon"
--         first :: rest ->
--             let
--                 -- Ensuring every vertex has a positive weight avoids a divide
--                 -- by zero and ensures that the result is strictly inside the
--                 -- polygon
--                 weightsGenerator =
--                     Fuzz.list numVertices (Fuzz.float 1 100)
--             in
--             weightsGenerator
--                 |> Fuzz.map
--                     (\weights ->
--                         let
--                             totalWeight =
--                                 List.sum weights
--                             weightedXCoordinates =
--                                 List.map2
--                                     (\weight vertex ->
--                                         Quantity.multiplyBy weight
--                                             (Point2d.xCoordinate vertex)
--                                     )
--                                     weights
--                                     vertices
--                             weightedYCoordinates =
--                                 List.map2
--                                     (\weight vertex ->
--                                         Quantity.multiplyBy weight
--                                             (Point2d.yCoordinate vertex)
--                                     )
--                                     weights
--                                     vertices
--                             x =
--                                 Quantity.sum weightedXCoordinates
--                                     |> Quantity.divideBy totalWeight
--                             y =
--                                 Quantity.sum weightedYCoordinates
--                                     |> Quantity.divideBy totalWeight
--                         in
--                         Point2d.xy x y
--                     )
--pointInPolygonClosestToCorrespondingVertex : Test
--pointInPolygonClosestToCorrespondingVertex =
--    let
--        description =
--            "Every point inside a Voronoi region must be closer to the corresponding vertex than any other vertex"
--        expectation points =
--            Expect.fail "TODO"
--    in
--    Test.check1 uniquePoints description expectation
