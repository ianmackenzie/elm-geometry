module Tesselation2d exposing (voronoiDiagram, delaunayTriangulation)

import Array exposing (Array)
import Dict
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Tesselation2d.Delaunay as Delaunay


delaunayTriangulation : Array Point2d -> List Triangle2d
delaunayTriangulation rawPoints =
    Delaunay.createGeometry rawPoints
        |> Delaunay.toTriangulation


voronoiDiagram : Array Point2d -> List Polygon2d
voronoiDiagram rawPoints =
    Delaunay.createGeometry rawPoints
        |> Delaunay.centerPointsPerVertex
        |> Dict.values
        |> List.map Polygon2d.convexHull
