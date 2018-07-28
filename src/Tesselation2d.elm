module Tesselation2d exposing (voronoiDiagram, delaunayTriangulation)

import Array exposing (Array)
import Dict
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Direction2d exposing (Direction2d)
import BoundingBox2d exposing (BoundingBox2d)
import Tesselation2d.Delaunay as Delaunay
import Tesselation2d.Voronoi as Voronoi exposing (Accumulator(..))
import TriangularMesh exposing (TriangularMesh)


delaunayTriangulation : Array Point2d -> TriangularMesh Point2d
delaunayTriangulation =
    delaunayTriangulationBy identity


delaunayTriangulationBy : (vertex -> Point2d) -> Array vertex -> TriangularMesh vertex
delaunayTriangulationBy toPoint2d rawPoints =
    Delaunay.createGeometry (Array.map toPoint2d rawPoints)
        |> Delaunay.toTriangularMesh rawPoints


type VoronoiRegion
    = Bounded Polygon2d
    | Unbounded (List Point2d) Direction2d Direction2d


voronoiDiagram : Array Point2d -> List { datapoint : Point2d, region : VoronoiRegion }
voronoiDiagram =
    voronoiDiagramBy identity


voronoiDiagramBy : (vertex -> Point2d) -> Array vertex -> List { datapoint : vertex, region : VoronoiRegion }
voronoiDiagramBy toPoint2d rawPoints =
    let
        helper ( index, centerPoints ) =
            Maybe.map2 (\point region -> { datapoint = point, region = region })
                (Array.get (index - 3) rawPoints)
                (case Voronoi.validatePointsAtInfinity (NoneAtInfinity []) centerPoints of
                    NoneAtInfinity vertices ->
                        Just (Bounded (Polygon2d.convexHull vertices))

                    TwoAtInfinity vertices atInfinity1 atInfinity2 ->
                        Just (Unbounded vertices atInfinity1 atInfinity2)

                    _ ->
                        Nothing
                )
    in
        Delaunay.createGeometry (Array.map toPoint2d rawPoints)
            |> Voronoi.centerPointsPerVertex
            |> Dict.toList
            |> List.filterMap helper
