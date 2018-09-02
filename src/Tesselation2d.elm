module Tesselation2d exposing (voronoiDiagram, delaunayTriangulation, VoronoiRegion(..))

import Array exposing (Array)
import Dict
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Direction2d exposing (Direction2d)
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
    | Unbounded Polyline2d ( Point2d, Direction2d ) ( Point2d, Direction2d )


constructUnboundedRegion : Point2d -> List Point2d -> ( Point2d, Direction2d ) -> ( Point2d, Direction2d ) -> Maybe VoronoiRegion
constructUnboundedRegion datapoint centerPoints atInfinity1 atInfinity2 =
    let
        toAngle point =
            Direction2d.from datapoint point
                |> Maybe.map Direction2d.toAngle
                |> Maybe.withDefault 0

        polyline =
            List.sortBy toAngle centerPoints
                |> Polyline2d.fromVertices
    in
        Just (Unbounded polyline atInfinity1 atInfinity2)


voronoiDiagram : Array Point2d -> List { datapoint : Point2d, region : VoronoiRegion }
voronoiDiagram =
    voronoiDiagramBy identity


voronoiDiagramBy : (vertex -> Point2d) -> Array vertex -> List { datapoint : vertex, region : VoronoiRegion }
voronoiDiagramBy toPoint2d rawPoints =
    let
        helper ( index, centerPoints ) =
            case Array.get (index - 3) rawPoints of
                Just datapoint ->
                    case Voronoi.validatePointsAtInfinity (toPoint2d datapoint) centerPoints (NoneAtInfinity []) of
                        NoneAtInfinity vertices ->
                            Just
                                { region = Bounded (Polygon2d.convexHull vertices)
                                , datapoint = datapoint
                                }

                        TwoAtInfinity vertices atInfinity1 atInfinity2 ->
                            constructUnboundedRegion (toPoint2d datapoint) vertices atInfinity1 atInfinity2
                                |> Maybe.map (\region -> { datapoint = datapoint, region = region })

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
    in
        Delaunay.createGeometry (Array.map toPoint2d rawPoints)
            |> Voronoi.centerPointsPerVertex
            |> Dict.toList
            |> List.filterMap helper
