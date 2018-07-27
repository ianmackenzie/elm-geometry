module Tesselation2d exposing (voronoiDiagram, delaunayTriangulation)

import Array exposing (Array)
import Dict
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Tesselation2d.Delaunay as Delaunay
import TriangularMesh exposing (TriangularMesh)


delaunayTriangulation : Array Point2d -> TriangularMesh Point2d
delaunayTriangulation =
    delaunayTriangulationBy identity


delaunayTriangulationBy : (vertex -> Point2d) -> Array vertex -> TriangularMesh vertex
delaunayTriangulationBy toPoint2d rawPoints =
    Delaunay.createGeometry (Array.map toPoint2d rawPoints)
        |> Delaunay.toTriangularMesh rawPoints


voronoiDiagram : Array Point2d -> List { datapoint : Point2d, polygon : Polygon2d }
voronoiDiagram =
    voronoiDiagramBy identity


voronoiDiagramBy : (vertex -> Point2d) -> Array vertex -> List { datapoint : vertex, polygon : Polygon2d }
voronoiDiagramBy toPoint2d rawPoints =
    let
        helper ( index, centerPoints ) =
            Maybe.map2 (\point polygon -> { datapoint = point, polygon = polygon })
                (Array.get (index - 3) rawPoints)
                (Just (Polygon2d.convexHull centerPoints))
    in
        Delaunay.createGeometry (Array.map toPoint2d rawPoints)
            |> Delaunay.centerPointsPerVertex
            |> Dict.toList
            |> List.filterMap helper
