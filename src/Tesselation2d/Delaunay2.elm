module Tesselation2d.Delaunay2 exposing (..)

import Array exposing (Array)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)


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


processFaces : List Face -> Int -> Vertex -> List Face -> Dict Int Edge -> ( List Face, List Edge )
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
            ( retainedFaces, Dict.values edgesByKey )


addNewFaces : Vertex -> List Edge -> List Face -> List Face
addNewFaces newVertex edges faces =
    case edges of
        firstEdge :: remainingEdges ->
            case firstEdge of
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

                                updatedFaces =
                                    newFace :: faces
                            in
                            addNewFaces newVertex remainingEdges updatedFaces

                        Nothing ->
                            addNewFaces newVertex remainingEdges faces

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

                                updatedFaces =
                                    newFace :: faces
                            in
                            addNewFaces newVertex remainingEdges updatedFaces

                        Nothing ->
                            addNewFaces newVertex remainingEdges faces

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

                                updatedFaces =
                                    newFace :: faces
                            in
                            addNewFaces newVertex remainingEdges updatedFaces

                        Nothing ->
                            addNewFaces newVertex remainingEdges faces

                OuterEdge edgeDirection firstOuterIndex secondOuterIndex ->
                    let
                        newFace =
                            OneVertexFace newVertex
                                firstOuterIndex
                                secondOuterIndex
                                edgeDirection

                        updatedFaces =
                            newFace :: faces
                    in
                    addNewFaces newVertex remainingEdges updatedFaces

        [] ->
            faces


addVertices : Int -> List Vertex -> List Face -> List Face
addVertices numVertices vertices faces =
    case vertices of
        firstVertex :: remainingVertices ->
            let
                ( retainedFaces, starEdges ) =
                    processFaces faces numVertices firstVertex [] Dict.empty

                updatedFaces =
                    addNewFaces firstVertex starEdges retainedFaces
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
