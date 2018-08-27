module VoronoiDiagram2d
    exposing
        ( CoincidentVertices(..)
        , VoronoiDiagram2d
        , empty
        , fromPoints
        , fromVerticesBy
        , insertPoint
        , insertVertexBy
        , polygons
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


type CoincidentVertices vertex
    = CoincidentVertices vertex vertex


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


type alias TrimBox =
    { boundingBox : BoundingBox2d
    , leftEdge : LineSegment2d
    , rightEdge : LineSegment2d
    , topEdge : LineSegment2d
    , bottomEdge : LineSegment2d
    , topLeftVertex : Point2d
    , topRightVertex : Point2d
    , bottomLeftVertex : Point2d
    , bottomRightVertex : Point2d
    }


addContainedPoint : BoundingBox2d -> Point2d -> List Point2d -> List Point2d
addContainedPoint boundingBox point accumulated =
    if BoundingBox2d.contains point boundingBox then
        point :: accumulated
    else
        accumulated


addContainedPoints : TrimBox -> List Point2d -> List Point2d -> List Point2d
addContainedPoints trimBox points accumulated =
    List.foldl (addContainedPoint trimBox.boundingBox) accumulated points


addEdgeIntersection : LineSegment2d -> LineSegment2d -> List Point2d -> List Point2d
addEdgeIntersection firstLineSegment secondLineSegment accumulated =
    case LineSegment2d.intersectionPoint firstLineSegment secondLineSegment of
        Just point ->
            point :: accumulated

        Nothing ->
            accumulated


addEdgeIntersections : TrimBox -> LineSegment2d -> List Point2d -> List Point2d
addEdgeIntersections trimBox lineSegment accumulated =
    accumulated
        |> addEdgeIntersection trimBox.leftEdge lineSegment
        |> addEdgeIntersection trimBox.rightEdge lineSegment
        |> addEdgeIntersection trimBox.topEdge lineSegment
        |> addEdgeIntersection trimBox.bottomEdge lineSegment


addAllEdgeIntersections : TrimBox -> List LineSegment2d -> List Point2d -> List Point2d
addAllEdgeIntersections trimBox lineSegments accumulated =
    List.foldl (addEdgeIntersections trimBox) accumulated lineSegments


addAxisIntersection : LineSegment2d -> Axis2d -> List Point2d -> List Point2d
addAxisIntersection lineSegment axis accumulated =
    case LineSegment2d.intersectionWithAxis axis lineSegment of
        Just point ->
            -- We only want points ahead of the axis' origin point, not behind
            if Point2d.signedDistanceAlong axis point >= 0 then
                point :: accumulated
            else
                accumulated

        Nothing ->
            accumulated


addAxisIntersections : TrimBox -> Axis2d -> List Point2d -> List Point2d
addAxisIntersections trimBox axis accumulated =
    accumulated
        |> addAxisIntersection trimBox.leftEdge axis
        |> addAxisIntersection trimBox.rightEdge axis
        |> addAxisIntersection trimBox.topEdge axis
        |> addAxisIntersection trimBox.bottomEdge axis


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


addPointInsideInfiniteRegion : Axis2d -> Axis2d -> List LineSegment2d -> Point2d -> List Point2d -> List Point2d
addPointInsideInfiniteRegion leftAxis rightAxis lineSegments point accumulated =
    if
        leftOf lineSegments point
            && (Point2d.signedDistanceFrom leftAxis point <= 0)
            && (Point2d.signedDistanceFrom rightAxis point >= 0)
    then
        point :: accumulated
    else
        accumulated


addPointsInsideInfiniteRegion : Axis2d -> Axis2d -> List LineSegment2d -> TrimBox -> List Point2d -> List Point2d
addPointsInsideInfiniteRegion leftAxis rightAxis lineSegments trimBox accumulated =
    accumulated
        |> addPointInsideInfiniteRegion
            leftAxis
            rightAxis
            lineSegments
            trimBox.topLeftVertex
        |> addPointInsideInfiniteRegion
            leftAxis
            rightAxis
            lineSegments
            trimBox.topRightVertex
        |> addPointInsideInfiniteRegion
            leftAxis
            rightAxis
            lineSegments
            trimBox.bottomLeftVertex
        |> addPointInsideInfiniteRegion
            leftAxis
            rightAxis
            lineSegments
            trimBox.bottomRightVertex


addPointInsideFiniteRegion : List LineSegment2d -> Point2d -> List Point2d -> List Point2d
addPointInsideFiniteRegion edges point accumulated =
    if leftOf edges point then
        point :: accumulated
    else
        accumulated


addPointsInsideFiniteRegion : List LineSegment2d -> TrimBox -> List Point2d -> List Point2d
addPointsInsideFiniteRegion edges trimBox accumulated =
    accumulated
        |> addPointInsideFiniteRegion edges trimBox.topLeftVertex
        |> addPointInsideFiniteRegion edges trimBox.topRightVertex
        |> addPointInsideFiniteRegion edges trimBox.bottomLeftVertex
        |> addPointInsideFiniteRegion edges trimBox.bottomRightVertex


deduplicate : List Point2d -> List Point2d
deduplicate points =
    case points of
        first :: rest ->
            deduplicateHelp first rest [ first ]

        [] ->
            []


deduplicateHelp : Point2d -> List Point2d -> List Point2d -> List Point2d
deduplicateHelp head rest accumulated =
    case rest of
        next :: remaining ->
            if head == next then
                deduplicateHelp head remaining accumulated
            else
                deduplicateHelp next remaining (next :: accumulated)

        [] ->
            accumulated


trimInfiniteRegion : TrimBox -> vertex -> Axis2d -> Axis2d -> Polyline2d -> Maybe ( vertex, Polygon2d )
trimInfiniteRegion trimBox vertex leftAxis rightAxis polyline =
    let
        polylineVertices =
            Polyline2d.vertices polyline

        polylineSegments =
            Polyline2d.segments polyline

        trimmedVertices =
            []
                |> addContainedPoints trimBox polylineVertices
                |> addAxisIntersections trimBox leftAxis
                |> addAxisIntersections trimBox rightAxis
                |> addAllEdgeIntersections trimBox polylineSegments
                |> addPointsInsideInfiniteRegion
                    leftAxis
                    rightAxis
                    polylineSegments
                    trimBox
    in
    case Point2d.centroid trimmedVertices of
        Just centroid ->
            let
                sortedVertices =
                    trimmedVertices
                        |> List.sortBy (pseudoAngle centroid >> negate)
                        |> deduplicate
            in
            Just ( vertex, Polygon2d.singleLoop sortedVertices )

        Nothing ->
            Nothing


trimFiniteRegion : TrimBox -> vertex -> Polygon2d -> Maybe ( vertex, Polygon2d )
trimFiniteRegion trimBox vertex polygon =
    case Polygon2d.boundingBox polygon of
        Just polygonBoundingBox ->
            if polygonBoundingBox |> BoundingBox2d.isContainedIn trimBox.boundingBox then
                Just ( vertex, polygon )
            else
                let
                    polygonVertices =
                        Polygon2d.vertices polygon

                    polygonEdges =
                        Polygon2d.edges polygon

                    trimmedVertices =
                        []
                            |> addContainedPoints trimBox polygonVertices
                            |> addAllEdgeIntersections trimBox polygonEdges
                            |> addPointsInsideFiniteRegion
                                polygonEdges
                                trimBox
                in
                case Point2d.centroid trimmedVertices of
                    Just centroid ->
                        let
                            sortedVertices =
                                trimmedVertices
                                    |> List.sortBy (pseudoAngle centroid >> negate)
                                    |> deduplicate
                        in
                        Just ( vertex, Polygon2d.singleLoop sortedVertices )

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing


trimRegion : TrimBox -> Region vertex -> Maybe ( vertex, Polygon2d )
trimRegion trimBox region =
    case region of
        Finite vertex polygon ->
            trimFiniteRegion trimBox vertex polygon

        Infinite vertex leftAxis rightAxis polyline ->
            trimInfiniteRegion trimBox vertex leftAxis rightAxis polyline


empty : VoronoiDiagram2d vertex
empty =
    VoronoiDiagram2d
        { delaunayTriangulation = DelaunayTriangulation2d.empty
        , regions = []
        }


fromPoints : Array Point2d -> Result (CoincidentVertices Point2d) (VoronoiDiagram2d Point2d)
fromPoints points =
    fromVerticesBy identity points


fromVerticesBy : (vertex -> Point2d) -> Array vertex -> Result (CoincidentVertices vertex) (VoronoiDiagram2d vertex)
fromVerticesBy getPosition givenVertices =
    case DelaunayTriangulation2d.fromVerticesBy getPosition givenVertices of
        Ok delaunayTriangulation ->
            let
                regions =
                    voronoiRegions delaunayTriangulation
            in
            Ok <|
                VoronoiDiagram2d
                    { delaunayTriangulation = delaunayTriangulation
                    , regions = regions
                    }

        Err (DelaunayTriangulation2d.CoincidentVertices firstVertex secondVertex) ->
            Err (CoincidentVertices firstVertex secondVertex)


insertPoint : Point2d -> VoronoiDiagram2d Point2d -> Result (CoincidentVertices Point2d) (VoronoiDiagram2d Point2d)
insertPoint point voronoiDiagram =
    insertVertexBy identity point voronoiDiagram


insertVertexBy : (vertex -> Point2d) -> vertex -> VoronoiDiagram2d vertex -> Result (CoincidentVertices vertex) (VoronoiDiagram2d vertex)
insertVertexBy getPosition vertex (VoronoiDiagram2d current) =
    case current.delaunayTriangulation |> DelaunayTriangulation2d.insertVertexBy getPosition vertex of
        Ok updatedTriangulation ->
            let
                updatedRegions =
                    voronoiRegions updatedTriangulation
            in
            Ok <|
                VoronoiDiagram2d
                    { delaunayTriangulation = updatedTriangulation
                    , regions = updatedRegions
                    }

        Err (DelaunayTriangulation2d.CoincidentVertices firstVertex secondVertex) ->
            Err (CoincidentVertices firstVertex secondVertex)


vertices : VoronoiDiagram2d vertex -> Array vertex
vertices (VoronoiDiagram2d voronoiDiagram) =
    DelaunayTriangulation2d.vertices voronoiDiagram.delaunayTriangulation


polygons : BoundingBox2d -> VoronoiDiagram2d vertex -> List ( vertex, Polygon2d )
polygons boundingBox (VoronoiDiagram2d voronoiDiagram) =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        topLeftVertex =
            Point2d.fromCoordinates ( minX, maxY )

        topRightVertex =
            Point2d.fromCoordinates ( maxX, maxY )

        bottomLeftVertex =
            Point2d.fromCoordinates ( minX, minY )

        bottomRightVertex =
            Point2d.fromCoordinates ( maxX, minY )

        trimBox =
            { boundingBox = boundingBox
            , topLeftVertex = topLeftVertex
            , topRightVertex = topRightVertex
            , bottomLeftVertex = bottomLeftVertex
            , bottomRightVertex = bottomRightVertex
            , leftEdge = LineSegment2d.from topLeftVertex bottomLeftVertex
            , bottomEdge = LineSegment2d.from bottomLeftVertex bottomRightVertex
            , rightEdge = LineSegment2d.from bottomRightVertex topRightVertex
            , topEdge = LineSegment2d.from topRightVertex topLeftVertex
            }
    in
    List.filterMap (trimRegion trimBox) voronoiDiagram.regions
