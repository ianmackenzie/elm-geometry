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


type OuterVertex
    = TopOuterVertex
    | LeftOuterVertex
    | RightOuterVertex


type OuterEdge
    = LeftOuterEdge
    | RightOuterEdge
    | BottomOuterEdge


type alias Vertex =
    { index : Int
    , position : Point2d
    }


type Face
    = ThreeVertexFace Vertex Vertex Vertex Circle2d
    | TwoVertexFace Vertex Vertex OuterVertex Axis2d
    | OneVertexFace Vertex OuterEdge Axis2d


type Edge
    = InnerEdge Vertex Vertex
    | InnerToOuterEdge Vertex OuterVertex
    | OuterToInnerEdge OuterVertex Vertex
    | OuterEdge OuterEdge


innerEdge : Int -> Vertex -> Vertex -> ( Int, Edge )
innerEdge numVertices firstVertex secondVertex =
    let
        i =
            firstVertex.index

        j =
            secondVertex.index

        key =
            if i <= j then
                i * numVertices + j
            else
                j * numVertices + i
    in
    ( key, InnerEdge firstVertex secondVertex )


mixedEdgeKey : Int -> Vertex -> OuterVertex -> Int
mixedEdgeKey numVertices vertex outerVertex =
    let
        i =
            vertex.index
    in
    case outerVertex of
        TopOuterVertex ->
            -numVertices + i

        LeftOuterVertex ->
            -2 * numVertices + i

        RightOuterVertex ->
            -3 * numVertices + i


innerToOuterEdge : Int -> Vertex -> OuterVertex -> ( Int, Edge )
innerToOuterEdge numVertices vertex outerVertex =
    ( mixedEdgeKey numVertices vertex outerVertex
    , InnerToOuterEdge vertex outerVertex
    )


outerToInnerEdge : Int -> OuterVertex -> Vertex -> ( Int, Edge )
outerToInnerEdge numVertices outerVertex vertex =
    ( mixedEdgeKey numVertices vertex outerVertex
    , OuterToInnerEdge outerVertex vertex
    )


outerEdge : Int -> OuterEdge -> ( Int, Edge )
outerEdge numVertices whichEdge =
    let
        key =
            case whichEdge of
                LeftOuterEdge ->
                    -4 * numVertices

                RightOuterEdge ->
                    -5 * numVertices

                BottomOuterEdge ->
                    -6 * numVertices
    in
    ( key, OuterEdge whichEdge )


contains : Point2d -> Face -> Bool
contains point face =
    case face of
        ThreeVertexFace _ _ _ circle ->
            Circle2d.contains point circle

        TwoVertexFace _ _ _ axis ->
            Point2d.signedDistanceFrom axis point > 0

        OneVertexFace _ _ axis ->
            Point2d.signedDistanceFrom axis point > 0


processFaces : List Face -> Int -> Vertex -> List Face -> List ( Int, Edge ) -> ( List Face, List ( Int, Edge ) )
processFaces faces numVertices newVertex retainedFaces accumulatedEdges =
    case faces of
        firstFace :: remainingFaces ->
            if contains newVertex.position firstFace then
                case firstFace of
                    ThreeVertexFace vertex1 vertex2 vertex3 _ ->
                        let
                            edge1 =
                                innerEdge numVertices vertex1 vertex2

                            edge2 =
                                innerEdge numVertices vertex2 vertex3

                            edge3 =
                                innerEdge numVertices vertex3 vertex1
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            (edge1 :: edge2 :: edge3 :: accumulatedEdges)

                    TwoVertexFace firstVertex secondVertex outerVertex _ ->
                        let
                            edge1 =
                                innerToOuterEdge numVertices
                                    secondVertex
                                    outerVertex

                            edge2 =
                                outerToInnerEdge numVertices
                                    outerVertex
                                    firstVertex

                            edge3 =
                                innerEdge numVertices firstVertex secondVertex
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            (edge1 :: edge2 :: edge3 :: accumulatedEdges)

                    OneVertexFace vertex LeftOuterEdge _ ->
                        let
                            edge1 =
                                innerToOuterEdge numVertices
                                    vertex
                                    TopOuterVertex

                            edge2 =
                                outerEdge numVertices LeftOuterEdge

                            edge3 =
                                outerToInnerEdge numVertices
                                    LeftOuterVertex
                                    vertex
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            (edge1 :: edge2 :: edge3 :: accumulatedEdges)

                    OneVertexFace vertex RightOuterEdge _ ->
                        let
                            edge1 =
                                innerToOuterEdge numVertices
                                    vertex
                                    RightOuterVertex

                            edge2 =
                                outerEdge numVertices RightOuterEdge

                            edge3 =
                                outerToInnerEdge numVertices
                                    TopOuterVertex
                                    vertex
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            (edge1 :: edge2 :: edge3 :: accumulatedEdges)

                    OneVertexFace vertex BottomOuterEdge _ ->
                        let
                            edge1 =
                                innerToOuterEdge numVertices
                                    vertex
                                    LeftOuterVertex

                            edge2 =
                                outerEdge numVertices BottomOuterEdge

                            edge3 =
                                outerToInnerEdge numVertices
                                    RightOuterVertex
                                    vertex
                        in
                        processFaces remainingFaces
                            numVertices
                            newVertex
                            retainedFaces
                            (edge1 :: edge2 :: edge3 :: accumulatedEdges)
            else
                processFaces remainingFaces
                    numVertices
                    newVertex
                    (firstFace :: retainedFaces)
                    accumulatedEdges

        [] ->
            ( retainedFaces, accumulatedEdges )


addEdge : ( Int, Edge ) -> Dict Int Edge -> Dict Int Edge
addEdge ( key, edge ) edgesByKey =
    edgesByKey
        |> Dict.update key
            (\entry ->
                case entry of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just edge
            )


collectStarEdges : List ( Int, Edge ) -> Dict Int Edge -> List Edge
collectStarEdges edges edgesByKey =
    case edges of
        firstEdge :: remainingEdges ->
            collectStarEdges remainingEdges (addEdge firstEdge edgesByKey)

        [] ->
            Dict.values edgesByKey


directionToOuterVertex : OuterVertex -> Direction2d
directionToOuterVertex outerVertex =
    case outerVertex of
        TopOuterVertex ->
            Direction2d.positiveY

        LeftOuterVertex ->
            Direction2d.negativeX

        RightOuterVertex ->
            Direction2d.positiveX


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

                InnerToOuterEdge vertex outerVertex ->
                    case Direction2d.from newVertex.position vertex.position of
                        Just direction ->
                            let
                                axis =
                                    Axis2d.through newVertex.position direction

                                newFace =
                                    TwoVertexFace
                                        newVertex
                                        vertex
                                        outerVertex
                                        axis

                                updatedFaces =
                                    newFace :: faces
                            in
                            addNewFaces newVertex remainingEdges updatedFaces

                        Nothing ->
                            addNewFaces newVertex remainingEdges faces

                OuterToInnerEdge outerVertex vertex ->
                    case Direction2d.from vertex.position newVertex.position of
                        Just direction ->
                            let
                                axis =
                                    Axis2d.through vertex.position direction

                                newFace =
                                    TwoVertexFace
                                        vertex
                                        newVertex
                                        outerVertex
                                        axis

                                updatedFaces =
                                    newFace :: faces
                            in
                            addNewFaces newVertex remainingEdges updatedFaces

                        Nothing ->
                            addNewFaces newVertex remainingEdges faces

                OuterEdge whichEdge ->
                    let
                        direction =
                            case whichEdge of
                                LeftOuterEdge ->
                                    Direction2d.positiveY

                                RightOuterEdge ->
                                    Direction2d.negativeY

                                BottomOuterEdge ->
                                    Direction2d.negativeX

                        axis =
                            Axis2d.through newVertex.position direction

                        newFace =
                            OneVertexFace newVertex whichEdge axis

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
                ( retainedFaces, accumulatedEdges ) =
                    processFaces faces numVertices firstVertex [] []

                starEdges =
                    collectStarEdges accumulatedEdges Dict.empty

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
    in
    case uniqueVertices of
        firstVertex :: remainingVertices ->
            let
                firstPoint =
                    firstVertex.position

                initialFaces =
                    [ OneVertexFace firstVertex
                        LeftOuterEdge
                        (Axis2d.through firstPoint Direction2d.positiveY)
                    , OneVertexFace firstVertex
                        RightOuterEdge
                        (Axis2d.through firstPoint Direction2d.negativeY)
                    , OneVertexFace firstVertex
                        BottomOuterEdge
                        (Axis2d.through firstPoint Direction2d.negativeX)
                    ]
            in
            addVertices numVertices remainingVertices initialFaces

        [] ->
            []
