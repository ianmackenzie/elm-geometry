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
    = Polygonal vertex Polygon2d
    | UShaped vertex Axis2d Axis2d Polyline2d
    | Strip vertex Axis2d Axis2d
    | HalfPlane vertex Axis2d
    | Unbounded vertex


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

                        polygonalRegion =
                            Polygonal delaunayVertex.vertex polygon
                    in
                    polygonalRegion :: accumulatedRegions

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

                                uShapedRegion =
                                    UShaped delaunayVertex.vertex
                                        leftAxis
                                        rightAxis
                                        polyline
                            in
                            uShapedRegion :: accumulatedRegions

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


addHalfAxisIntersection : LineSegment2d -> Axis2d -> List Point2d -> List Point2d
addHalfAxisIntersection lineSegment axis accumulated =
    case LineSegment2d.intersectionWithAxis axis lineSegment of
        Just point ->
            -- We only want points ahead of the axis' origin point, not behind
            if Point2d.signedDistanceAlong axis point >= 0 then
                point :: accumulated
            else
                accumulated

        Nothing ->
            accumulated


addFullAxisIntersection : LineSegment2d -> Axis2d -> List Point2d -> List Point2d
addFullAxisIntersection lineSegment axis accumulated =
    case LineSegment2d.intersectionWithAxis axis lineSegment of
        Just point ->
            point :: accumulated

        Nothing ->
            accumulated


addHalfAxisIntersections : TrimBox -> Axis2d -> List Point2d -> List Point2d
addHalfAxisIntersections trimBox axis accumulated =
    accumulated
        |> addHalfAxisIntersection trimBox.leftEdge axis
        |> addHalfAxisIntersection trimBox.rightEdge axis
        |> addHalfAxisIntersection trimBox.topEdge axis
        |> addHalfAxisIntersection trimBox.bottomEdge axis


addFullAxisIntersections : TrimBox -> Axis2d -> List Point2d -> List Point2d
addFullAxisIntersections trimBox axis accumulated =
    accumulated
        |> addFullAxisIntersection trimBox.leftEdge axis
        |> addFullAxisIntersection trimBox.rightEdge axis
        |> addFullAxisIntersection trimBox.topEdge axis
        |> addFullAxisIntersection trimBox.bottomEdge axis


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


deduplicateAndReverse : List Point2d -> List Point2d
deduplicateAndReverse points =
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


constructPolygon : vertex -> List Point2d -> Maybe ( vertex, Polygon2d )
constructPolygon vertex points =
    case Point2d.centroid points of
        Just centroid ->
            let
                sortedPoints =
                    points
                        |> List.sortBy (pseudoAngle centroid >> negate)
                        |> deduplicateAndReverse
            in
            Just ( vertex, Polygon2d.singleLoop sortedPoints )

        Nothing ->
            Nothing


trimUShapedRegion : TrimBox -> vertex -> Axis2d -> Axis2d -> Polyline2d -> Maybe ( vertex, Polygon2d )
trimUShapedRegion trimBox vertex leftAxis rightAxis polyline =
    let
        polylineVertices =
            Polyline2d.vertices polyline

        polylineSegments =
            Polyline2d.segments polyline

        trimmedVertices =
            []
                |> addContainedPoints trimBox polylineVertices
                |> addHalfAxisIntersections trimBox leftAxis
                |> addHalfAxisIntersections trimBox rightAxis
                |> addAllEdgeIntersections trimBox polylineSegments
                |> addPointsInsideInfiniteRegion
                    leftAxis
                    rightAxis
                    polylineSegments
                    trimBox
    in
    constructPolygon vertex trimmedVertices


trimPolygonalRegion : TrimBox -> vertex -> Polygon2d -> Maybe ( vertex, Polygon2d )
trimPolygonalRegion trimBox vertex polygon =
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
                constructPolygon vertex trimmedVertices

        Nothing ->
            Nothing


addPointBetweenAxes : Axis2d -> Axis2d -> Point2d -> List Point2d -> List Point2d
addPointBetweenAxes leftAxis rightAxis point accumulated =
    if
        (Point2d.signedDistanceFrom leftAxis point <= 0)
            && (Point2d.signedDistanceFrom rightAxis point >= 0)
    then
        point :: accumulated
    else
        accumulated


trimStripRegion : TrimBox -> vertex -> Axis2d -> Axis2d -> Maybe ( vertex, Polygon2d )
trimStripRegion trimBox vertex leftAxis rightAxis =
    let
        trimmedVertices =
            []
                |> addFullAxisIntersections trimBox leftAxis
                |> addFullAxisIntersections trimBox rightAxis
                |> addPointBetweenAxes
                    leftAxis
                    rightAxis
                    trimBox.bottomLeftVertex
                |> addPointBetweenAxes
                    leftAxis
                    rightAxis
                    trimBox.bottomRightVertex
                |> addPointBetweenAxes
                    leftAxis
                    rightAxis
                    trimBox.topRightVertex
                |> addPointBetweenAxes
                    leftAxis
                    rightAxis
                    trimBox.topLeftVertex
    in
    constructPolygon vertex trimmedVertices


addPointBesideAxis : Axis2d -> Point2d -> List Point2d -> List Point2d
addPointBesideAxis leftAxis point accumulated =
    if Point2d.signedDistanceFrom leftAxis point <= 0 then
        point :: accumulated
    else
        accumulated


trimHalfPlane : TrimBox -> vertex -> Axis2d -> Maybe ( vertex, Polygon2d )
trimHalfPlane trimBox vertex leftAxis =
    let
        trimmedVertices =
            []
                |> addFullAxisIntersections trimBox leftAxis
                |> addPointBesideAxis leftAxis trimBox.bottomLeftVertex
                |> addPointBesideAxis leftAxis trimBox.bottomRightVertex
                |> addPointBesideAxis leftAxis trimBox.topRightVertex
                |> addPointBesideAxis leftAxis trimBox.topLeftVertex
    in
    constructPolygon vertex trimmedVertices


trimRegion : TrimBox -> Region vertex -> Maybe ( vertex, Polygon2d )
trimRegion trimBox region =
    case region of
        Polygonal vertex polygon ->
            trimPolygonalRegion trimBox vertex polygon

        UShaped vertex leftAxis rightAxis polyline ->
            trimUShapedRegion trimBox vertex leftAxis rightAxis polyline

        Strip vertex leftAxis rightAxis ->
            trimStripRegion trimBox vertex leftAxis rightAxis

        HalfPlane vertex leftAxis ->
            trimHalfPlane trimBox vertex leftAxis

        Unbounded vertex ->
            let
                boundingBoxRectangle =
                    Polygon2d.singleLoop
                        [ trimBox.bottomLeftVertex
                        , trimBox.bottomRightVertex
                        , trimBox.topRightVertex
                        , trimBox.topLeftVertex
                        ]
            in
            Just ( vertex, boundingBoxRectangle )


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
