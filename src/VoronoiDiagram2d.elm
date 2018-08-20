module VoronoiDiagram2d
    exposing
        ( VoronoiDiagram2d
        , empty
        , fromPoints
        , fromVerticesBy
        , insertPoint
        , insertVertexBy
        , vertices
        )

import Array exposing (Array)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (DelaunayFace(..), DelaunayVertex)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)


type Region vertex
    = Finite vertex Polygon2d
    | Infinite vertex Axis2d Axis2d Polyline2d


type VoronoiDiagram2d vertex
    = VoronoiDiagram2d
        { delaunayTriangulation : DelaunayTriangulation2d vertex
        , regions : List (Region vertex)
        }


type alias Accumulator =
    { points : List Point2d
    , startDirection : Maybe Direction2d
    , endDirection : Maybe Direction2d
    }


addInterior : Point2d -> Maybe Accumulator -> Maybe Accumulator
addInterior point entry =
    case entry of
        Just accumulator ->
            Just
                { accumulator
                    | points = point :: accumulator.points
                }

        Nothing ->
            Just
                { points = [ point ]
                , startDirection = Nothing
                , endDirection = Nothing
                }


addStartDirection : Direction2d -> Maybe Accumulator -> Maybe Accumulator
addStartDirection direction entry =
    case entry of
        Just accumulator ->
            Just { accumulator | startDirection = Just direction }

        Nothing ->
            Just
                { points = []
                , startDirection = Just direction
                , endDirection = Nothing
                }


addEndDirection : Direction2d -> Maybe Accumulator -> Maybe Accumulator
addEndDirection direction entry =
    case entry of
        Just accumulator ->
            Just { accumulator | endDirection = Just direction }

        Nothing ->
            Just
                { points = []
                , startDirection = Nothing
                , endDirection = Just direction
                }


updateAccumulators : DelaunayFace vertex -> Dict Int Accumulator -> Dict Int Accumulator
updateAccumulators face accumulators =
    case face of
        ThreeVertexFace firstVertex secondVertex thirdVertex circumcircle ->
            let
                centerPoint =
                    Circle2d.centerPoint circumcircle
            in
            accumulators
                |> Dict.update firstVertex.index (addInterior centerPoint)
                |> Dict.update secondVertex.index (addInterior centerPoint)
                |> Dict.update thirdVertex.index (addInterior centerPoint)

        TwoVertexFace firstVertex secondVertex _ edgeDirection ->
            let
                point =
                    Point2d.midpoint firstVertex.position secondVertex.position

                direction =
                    Direction2d.rotateCounterclockwise edgeDirection
            in
            accumulators
                |> Dict.update firstVertex.index (addEndDirection direction)
                |> Dict.update secondVertex.index (addStartDirection direction)

        OneVertexFace _ _ _ _ ->
            -- Infinite triangles with only one actual vertex do not
            -- contribute to the Voronoi regions
            accumulators


pseudoAngle : Point2d -> Point2d -> Float
pseudoAngle startPoint endPoint =
    let
        ( x0, y0 ) =
            Point2d.coordinates startPoint

        ( x1, y1 ) =
            Point2d.coordinates endPoint

        dx =
            x1 - x0

        dy =
            y1 - y0

        p =
            dx / (abs dx + abs dy)
    in
    if dy < 0 then
        p - 1
    else
        1 - p


collectRegions : Dict Int Accumulator -> DelaunayVertex vertex -> List (Region vertex) -> List (Region vertex)
collectRegions accumulatorsByIndex delaunayVertex accumulatedRegions =
    case Dict.get delaunayVertex.index accumulatorsByIndex of
        Just { points, startDirection, endDirection } ->
            case ( startDirection, endDirection ) of
                ( Nothing, Nothing ) ->
                    let
                        sortedPoints =
                            points
                                |> List.sortBy
                                    (pseudoAngle delaunayVertex.position)

                        polygon =
                            Polygon2d.singleLoop sortedPoints

                        finiteRegion =
                            Finite delaunayVertex.vertex polygon
                    in
                    finiteRegion :: accumulatedRegions

                ( Just startDirection_, Just endDirection_ ) ->
                    let
                        sortDirection =
                            startDirection_ |> Direction2d.rotateClockwise

                        sortAxis =
                            Axis2d.through delaunayVertex.position sortDirection

                        sortedPoints =
                            points
                                |> List.sortBy
                                    (Point2d.signedDistanceAlong sortAxis)
                    in
                    case sortedPoints of
                        startPoint :: remainingPoints ->
                            let
                                endPoint =
                                    List.foldl always
                                        startPoint
                                        remainingPoints

                                leftAxis =
                                    Axis2d.through startPoint startDirection_

                                rightAxis =
                                    Axis2d.through endPoint endDirection_

                                polyline =
                                    Polyline2d.fromVertices sortedPoints

                                infiniteRegion =
                                    Infinite delaunayVertex.vertex
                                        leftAxis
                                        rightAxis
                                        polyline
                            in
                            infiniteRegion :: accumulatedRegions

                        [] ->
                            accumulatedRegions

                _ ->
                    accumulatedRegions

        Nothing ->
            accumulatedRegions


voronoiRegions : DelaunayTriangulation2d vertex -> List (Region vertex)
voronoiRegions delaunayTriangulation =
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            []

        Types.DelaunayTriangulation2d triangulation ->
            let
                accumulatorsByIndex =
                    List.foldl updateAccumulators Dict.empty triangulation.faces
            in
            List.foldl (collectRegions accumulatorsByIndex)
                []
                triangulation.delaunayVertices


type alias TrimRegion =
    { boundingBox : BoundingBox2d
    , leftEdge : LineSegment2d
    , rightEdge : LineSegment2d
    , topEdge : LineSegment2d
    , bottomEdge : LineSegment2d
    }


addContainedPoint : TrimRegion -> Point2d -> List Point2d -> List Point2d
addContainedPoint trimRegion point accumulated =
    if BoundingBox2d.contains point trimRegion.boundingBox then
        point :: accumulated
    else
        accumulated


addEdgeIntersection : LineSegment2d -> LineSegment2d -> List Point2d -> List Point2d
addEdgeIntersection firstLineSegment secondLineSegment accumulated =
    case LineSegment2d.intersectionPoint firstLineSegment secondLineSegment of
        Just point ->
            point :: accumulated

        Nothing ->
            accumulated


addEdgeIntersections : TrimRegion -> LineSegment2d -> List Point2d -> List Point2d
addEdgeIntersections trimRegion lineSegment accumulated =
    accumulated
        |> addEdgeIntersection trimRegion.leftEdge lineSegment
        |> addEdgeIntersection trimRegion.rightEdge lineSegment
        |> addEdgeIntersection trimRegion.topEdge lineSegment
        |> addEdgeIntersection trimRegion.bottomEdge lineSegment


addAxisIntersection : LineSegment2d -> Axis2d -> List Point2d -> List Point2d
addAxisIntersection lineSegment axis accumulated =
    case LineSegment2d.intersectionWithAxis axis lineSegment of
        Just point ->
            point :: accumulated

        Nothing ->
            accumulated


leftOfSegment : LineSegment2d -> Point2d -> Bool
leftOfSegment lineSegment point =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x, y ) =
            Point2d.coordinates point
    in
    (x - x1) * (y1 - y2) + (y - y1) * (x2 - x1) >= 0


leftOf : List LineSegment2d -> Point2d -> Bool
leftOf lineSegments point =
    case lineSegments of
        first :: rest ->
            if leftOfSegment first point then
                leftOf rest point
            else
                False

        [] ->
            True


trimInfiniteRegion : TrimRegion -> Axis2d -> Axis2d -> Polyline2d -> Maybe Polygon2d
trimInfiniteRegion trimRegion leftAxis rightAxis polyline =
    Nothing


trimFiniteRegion : TrimRegion -> Polygon2d -> Maybe Polygon2d
trimFiniteRegion trimRegion polygon =
    Nothing


empty : VoronoiDiagram2d vertex
empty =
    VoronoiDiagram2d
        { delaunayTriangulation = DelaunayTriangulation2d.empty
        , regions = []
        }


fromPoints : Array Point2d -> VoronoiDiagram2d Point2d
fromPoints points =
    fromVerticesBy identity points


fromVerticesBy : (vertex -> Point2d) -> Array vertex -> VoronoiDiagram2d vertex
fromVerticesBy getPosition givenVertices =
    let
        delaunayTriangulation =
            DelaunayTriangulation2d.fromVerticesBy getPosition givenVertices

        regions =
            voronoiRegions delaunayTriangulation
    in
    VoronoiDiagram2d
        { delaunayTriangulation = delaunayTriangulation
        , regions = regions
        }


insertPoint : Point2d -> VoronoiDiagram2d Point2d -> VoronoiDiagram2d Point2d
insertPoint point voronoiDiagram =
    insertVertexBy identity point voronoiDiagram


insertVertexBy : (vertex -> Point2d) -> vertex -> VoronoiDiagram2d vertex -> VoronoiDiagram2d vertex
insertVertexBy getPosition vertex (VoronoiDiagram2d current) =
    let
        updatedTriangulation =
            current.delaunayTriangulation
                |> DelaunayTriangulation2d.insertVertexBy getPosition vertex

        updatedRegions =
            voronoiRegions updatedTriangulation
    in
    VoronoiDiagram2d
        { delaunayTriangulation = updatedTriangulation
        , regions = updatedRegions
        }


vertices : VoronoiDiagram2d vertex -> Array vertex
vertices (VoronoiDiagram2d voronoiDiagram) =
    DelaunayTriangulation2d.vertices voronoiDiagram.delaunayTriangulation
