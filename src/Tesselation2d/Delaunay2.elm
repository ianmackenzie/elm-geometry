module Tesselation2d.Delaunay2 exposing (..)

import Array exposing (Array)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)


-- DELAUNAY TRIANGULATION


type alias Vertex =
    { index : Int
    , position : Point2d
    }


type Face
    = ThreeVertexFace Vertex Vertex Vertex Circle2d
    | TwoVertexFace Vertex Vertex Int Direction2d
    | OneVertexFace Vertex Int Int Direction2d


type Edge
    = InnerEdge Vertex Vertex
    | InnerToOuterEdge Vertex Int
    | OuterToInnerEdge Int Vertex
    | OuterEdge Direction2d Int Int


edgeKey : Int -> Int -> Int -> Int
edgeKey numVertices i j =
    if i <= j then
        i * numVertices + j
    else
        j * numVertices + i


signedDistance : Point2d -> Point2d -> Direction2d -> Float
signedDistance point vertexPosition edgeDirection =
    let
        ( x, y ) =
            Point2d.coordinates point

        ( x0, y0 ) =
            Point2d.coordinates vertexPosition

        ( dx, dy ) =
            Direction2d.components edgeDirection
    in
    (y - y0) * dx - (x - x0) * dy


addEdge : Edge -> Maybe Edge -> Maybe Edge
addEdge newEdge maybeEdge =
    case maybeEdge of
        Just edge ->
            Nothing

        Nothing ->
            Just newEdge


processFaces : List Face -> Int -> Vertex -> List Face -> Dict Int Edge -> ( List Face, Dict Int Edge )
processFaces faces numVertices newVertex retainedFaces edgesByKey =
    case faces of
        firstFace :: remainingFaces ->
            case firstFace of
                ThreeVertexFace firstVertex secondVertex thirdVertex circumcircle ->
                    if Circle2d.contains newVertex.position circumcircle then
                        let
                            firstIndex =
                                firstVertex.index

                            secondIndex =
                                secondVertex.index

                            thirdIndex =
                                thirdVertex.index

                            key1 =
                                edgeKey numVertices firstIndex secondIndex

                            edge1 =
                                InnerEdge firstVertex secondVertex

                            key2 =
                                edgeKey numVertices secondIndex thirdIndex

                            edge2 =
                                InnerEdge secondVertex thirdVertex

                            key3 =
                                edgeKey numVertices thirdIndex firstIndex

                            edge3 =
                                InnerEdge thirdVertex firstVertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            (firstFace :: retainedFaces)
                            edgesByKey

                TwoVertexFace firstVertex secondVertex outerIndex edgeDirection ->
                    let
                        insideInfiniteCircle =
                            signedDistance
                                newVertex.position
                                firstVertex.position
                                edgeDirection
                                > 0
                    in
                    if insideInfiniteCircle then
                        let
                            firstIndex =
                                firstVertex.index

                            secondIndex =
                                secondVertex.index

                            key1 =
                                edgeKey numVertices firstIndex secondIndex

                            edge1 =
                                InnerEdge firstVertex secondVertex

                            key2 =
                                edgeKey numVertices secondIndex outerIndex

                            edge2 =
                                InnerToOuterEdge secondVertex outerIndex

                            key3 =
                                edgeKey numVertices outerIndex firstIndex

                            edge3 =
                                OuterToInnerEdge outerIndex firstVertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            (firstFace :: retainedFaces)
                            edgesByKey

                OneVertexFace vertex firstOuterIndex secondOuterIndex edgeDirection ->
                    let
                        insideInfiniteCircle =
                            signedDistance
                                newVertex.position
                                vertex.position
                                edgeDirection
                                < 0
                    in
                    if insideInfiniteCircle then
                        let
                            vertexIndex =
                                vertex.index

                            key1 =
                                edgeKey numVertices vertexIndex firstOuterIndex

                            edge1 =
                                InnerToOuterEdge vertex firstOuterIndex

                            key2 =
                                edgeKey numVertices
                                    firstOuterIndex
                                    secondOuterIndex

                            edge2 =
                                OuterEdge edgeDirection
                                    firstOuterIndex
                                    secondOuterIndex

                            key3 =
                                edgeKey numVertices secondOuterIndex vertexIndex

                            edge3 =
                                OuterToInnerEdge secondOuterIndex vertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            (firstFace :: retainedFaces)
                            edgesByKey

        [] ->
            ( retainedFaces, edgesByKey )


addNewFace : Vertex -> Int -> Edge -> List Face -> List Face
addNewFace newVertex _ edge faces =
    case edge of
        InnerEdge firstVertex secondVertex ->
            let
                maybeCircumcircle =
                    Circle2d.throughPoints
                        newVertex.position
                        firstVertex.position
                        secondVertex.position
            in
            case maybeCircumcircle of
                Just circumcircle ->
                    let
                        newFace =
                            ThreeVertexFace
                                newVertex
                                firstVertex
                                secondVertex
                                circumcircle
                    in
                    newFace :: faces

                Nothing ->
                    faces

        InnerToOuterEdge vertex outerIndex ->
            case Direction2d.from newVertex.position vertex.position of
                Just edgeDirection ->
                    let
                        newFace =
                            TwoVertexFace
                                newVertex
                                vertex
                                outerIndex
                                edgeDirection
                    in
                    newFace :: faces

                Nothing ->
                    faces

        OuterToInnerEdge outerIndex vertex ->
            case Direction2d.from vertex.position newVertex.position of
                Just edgeDirection ->
                    let
                        newFace =
                            TwoVertexFace
                                vertex
                                newVertex
                                outerIndex
                                edgeDirection
                    in
                    newFace :: faces

                Nothing ->
                    faces

        OuterEdge edgeDirection firstOuterIndex secondOuterIndex ->
            let
                newFace =
                    OneVertexFace newVertex
                        firstOuterIndex
                        secondOuterIndex
                        edgeDirection
            in
            newFace :: faces


addVertices : Int -> List Vertex -> List Face -> List Face
addVertices numVertices vertices faces =
    case vertices of
        firstVertex :: remainingVertices ->
            let
                ( retainedFaces, starEdges ) =
                    processFaces faces numVertices firstVertex [] Dict.empty

                updatedFaces =
                    Dict.foldl (addNewFace firstVertex) retainedFaces starEdges
            in
            addVertices numVertices remainingVertices updatedFaces

        [] ->
            faces


insertVertex : Vertex -> Dict ( Float, Float ) Vertex -> Dict ( Float, Float ) Vertex
insertVertex vertex dict =
    Dict.insert (Point2d.coordinates vertex.position) vertex dict


triangulation : Array Point2d -> List Face
triangulation points =
    let
        vertices =
            points |> Array.toList |> List.indexedMap Vertex

        numVertices =
            List.length vertices

        uniqueVertices =
            List.foldl insertVertex Dict.empty vertices
                |> Dict.values

        topOuterIndex =
            numVertices

        leftOuterIndex =
            numVertices + 1

        rightOuterIndex =
            numVertices + 2

        totalNumVertices =
            numVertices + 3
    in
    case uniqueVertices of
        firstVertex :: remainingVertices ->
            let
                firstPoint =
                    firstVertex.position

                initialFaces =
                    [ OneVertexFace firstVertex
                        topOuterIndex
                        leftOuterIndex
                        Direction2d.positiveY
                    , OneVertexFace firstVertex
                        leftOuterIndex
                        rightOuterIndex
                        Direction2d.negativeX
                    , OneVertexFace firstVertex
                        rightOuterIndex
                        topOuterIndex
                        Direction2d.negativeY
                    ]
            in
            addVertices totalNumVertices remainingVertices initialFaces

        [] ->
            []



-- VORONOI REGIONS


type VoronoiRegion
    = FiniteRegion Polygon2d
    | InfiniteRegion Polyline2d Direction2d Direction2d


type alias VoronoiAccumulator =
    { interiorPoints : List Point2d
    , startAxis : Maybe Axis2d
    , endAxis : Maybe Axis2d
    }


addInterior : Point2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
addInterior point entry =
    case entry of
        Just accumulator ->
            Just
                { accumulator
                    | interiorPoints = point :: accumulator.interiorPoints
                }

        Nothing ->
            Just
                { interiorPoints = [ point ]
                , startAxis = Nothing
                , endAxis = Nothing
                }


addStartAxis : Axis2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
addStartAxis axis entry =
    case entry of
        Just accumulator ->
            Just { accumulator | startAxis = Just axis }

        Nothing ->
            Just
                { interiorPoints = []
                , startAxis = Just axis
                , endAxis = Nothing
                }


addEndAxis : Axis2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
addEndAxis axis entry =
    case entry of
        Just accumulator ->
            Just { accumulator | endAxis = Just axis }

        Nothing ->
            Just
                { interiorPoints = []
                , startAxis = Nothing
                , endAxis = Just axis
                }


accumulateRegions : Face -> Dict Int VoronoiAccumulator -> Dict Int VoronoiAccumulator
accumulateRegions face accumulators =
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

                axis =
                    Axis2d.through point direction
            in
            accumulators
                |> Dict.update firstVertex.index (addEndAxis axis)
                |> Dict.update secondVertex.index (addStartAxis axis)

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


createRegion : Vertex -> Dict Int VoronoiAccumulator -> VoronoiRegion
createRegion vertex accumulatorsByIndex =
    case Dict.get vertex.index accumulatorsByIndex of
        Just { interiorPoints, startAxis, endAxis } ->
            case ( startAxis, endAxis ) of
                ( Nothing, Nothing ) ->
                    let
                        sortedPoints =
                            interiorPoints
                                |> List.sortBy (pseudoAngle vertex.position)
                    in
                    FiniteRegion (Polygon2d.singleLoop sortedPoints)

                ( Just startAxis_, Just endAxis_ ) ->
                    let
                        startPoint =
                            Axis2d.originPoint startAxis_

                        endPoint =
                            Axis2d.originPoint endAxis_
                    in
                    case Direction2d.from startPoint endPoint of
                        Just direction ->
                            let
                                axis =
                                    Axis2d.through startPoint direction

                                sortedPoints =
                                    (endPoint :: interiorPoints)
                                        |> List.sortBy
                                            (Point2d.signedDistanceAlong axis)

                                polyline =
                                    Polyline2d.fromVertices
                                        (startPoint :: sortedPoints)
                            in
                            InfiniteRegion polyline
                                (Axis2d.direction startAxis_)
                                (Axis2d.direction endAxis_)

                        Nothing ->
                            FiniteRegion (Polygon2d.singleLoop [])

                _ ->
                    FiniteRegion (Polygon2d.singleLoop [])

        Nothing ->
            FiniteRegion (Polygon2d.singleLoop [])
