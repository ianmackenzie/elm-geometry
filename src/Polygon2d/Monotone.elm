--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polygon2d.Monotone exposing
    ( faces
    , init
    , monotonePolygons
    , triangulation
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Geometry.Types exposing (Polygon(..))
import LineSegment2d
import Point2d exposing (Point2d)
import Polygon2d.EdgeSet as EdgeSet exposing (EdgeSet)
import Quantity exposing (Quantity, Squared)
import Quantity.Extra as Quantity
import Set exposing (Set)
import Triangle2d
import TriangularMesh exposing (TriangularMesh)


type Kind
    = Start
    | End
    | Left
    | Right
    | Split
    | Merge


type alias VertexRecord vertex units coordinates =
    { vertex : vertex
    , position : Point2d units coordinates
    , kind : Kind
    }


comparePoints : Point2d units coordinates -> Point2d units coordinates -> Order
comparePoints p1 p2 =
    let
        x1 =
            Point2d.xCoordinate p1

        y1 =
            Point2d.yCoordinate p1

        x2 =
            Point2d.xCoordinate p2

        y2 =
            Point2d.yCoordinate p2
    in
    if y1 |> Quantity.lessThan y2 then
        LT

    else if y1 |> Quantity.greaterThan y2 then
        GT

    else
        Quantity.compare x2 x1


leftTurn : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Bool
leftTurn p1 p2 p3 =
    let
        x1 =
            Point2d.xCoordinate p1

        y1 =
            Point2d.yCoordinate p1

        x2 =
            Point2d.xCoordinate p2

        y2 =
            Point2d.yCoordinate p2

        x3 =
            Point2d.xCoordinate p3

        y3 =
            Point2d.yCoordinate p3

        firstProduct =
            (x2 |> Quantity.minus x1)
                |> Quantity.times
                    (y3 |> Quantity.minus y2)

        secondProduct =
            (y2 |> Quantity.minus y1)
                |> Quantity.times
                    (x3 |> Quantity.minus x2)

        difference =
            firstProduct |> Quantity.minus secondProduct
    in
    difference |> Quantity.greaterThan Quantity.zero


kind : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Kind
kind previous current next =
    let
        compareToPrevious =
            comparePoints current previous

        compareToNext =
            comparePoints current next
    in
    if compareToPrevious == GT && compareToNext == GT then
        if leftTurn previous current next then
            Start

        else
            Split

    else if compareToPrevious == LT && compareToNext == LT then
        if leftTurn previous current next then
            End

        else
            Merge

    else if compareToPrevious == GT then
        Right

    else
        Left


toVertexRecords : (vertex -> Point2d units coordinates) -> List vertex -> List (VertexRecord vertex units coordinates)
toVertexRecords getPosition vertices =
    case vertices of
        [] ->
            []

        [ singleVertex ] ->
            -- TODO use as combined split/merge vertex
            []

        [ firstVertex, secondVertex ] ->
            let
                firstPoint =
                    getPosition firstVertex

                secondPoint =
                    getPosition secondVertex
            in
            if comparePoints firstPoint secondPoint == GT then
                [ { vertex = firstVertex, position = firstPoint, kind = Split }
                , { vertex = secondVertex, position = secondPoint, kind = Merge }
                ]

            else
                [ { vertex = firstVertex, position = firstPoint, kind = Merge }
                , { vertex = secondVertex, position = secondPoint, kind = Split }
                ]

        firstVertex :: secondVertex :: thirdVertex :: rest ->
            let
                firstPoint =
                    getPosition firstVertex

                secondPoint =
                    getPosition secondVertex

                thirdPoint =
                    getPosition thirdVertex

                lastVertex =
                    List.foldl always thirdVertex rest

                lastPoint =
                    getPosition lastVertex

                collect previousVertex previousPoint currentVertex currentPoint remainingVertices accumulated =
                    case remainingVertices of
                        [] ->
                            let
                                lastVertexRecord =
                                    { vertex = currentVertex
                                    , position = currentPoint
                                    , kind =
                                        kind
                                            previousPoint
                                            currentPoint
                                            firstPoint
                                    }
                            in
                            List.reverse (lastVertexRecord :: accumulated)

                        nextVertex :: followingVertices ->
                            let
                                nextPoint =
                                    getPosition nextVertex

                                newVertexRecord =
                                    { vertex = currentVertex
                                    , position = currentPoint
                                    , kind =
                                        kind
                                            previousPoint
                                            currentPoint
                                            nextPoint
                                    }
                            in
                            collect
                                currentVertex
                                currentPoint
                                nextVertex
                                nextPoint
                                followingVertices
                                (newVertexRecord :: accumulated)
            in
            collect lastVertex lastPoint firstVertex firstPoint (secondVertex :: thirdVertex :: rest) []


type alias Edge =
    { startVertexIndex : Int
    , endVertexIndex : Int
    , nextEdgeIndex : Int
    , previousEdgeIndex : Int
    }


type alias Loops vertex units coordinates =
    { vertexRecords : List (VertexRecord vertex units coordinates)
    , edges : Array Edge
    }


removeDuplicates : (vertex -> Point2d units coordinates) -> List vertex -> List vertex
removeDuplicates getPosition vertices =
    case vertices of
        [] ->
            []

        firstVertex :: rest ->
            let
                -- Strip out adjacent duplicates
                accumulatedVertices =
                    accumulateDistinctVertices getPosition firstVertex rest []
            in
            case accumulatedVertices of
                lastVertex :: otherVertices ->
                    if getPosition lastVertex == getPosition firstVertex then
                        -- Drop the last point since it's equal to the
                        -- first
                        firstVertex :: List.reverse otherVertices

                    else
                        -- Keep all points
                        firstVertex :: List.reverse accumulatedVertices

                [] ->
                    -- Just have the first vertex
                    [ firstVertex ]


accumulateDistinctVertices : (vertex -> Point2d units coordinates) -> vertex -> List vertex -> List vertex -> List vertex
accumulateDistinctVertices getPosition previousVertex vertices accumulatedVertices =
    case vertices of
        [] ->
            accumulatedVertices

        vertex :: rest ->
            let
                updatedVertices =
                    if getPosition vertex == getPosition previousVertex then
                        accumulatedVertices

                    else
                        vertex :: accumulatedVertices
            in
            accumulateDistinctVertices getPosition vertex rest updatedVertices


init : (vertex -> Point2d units coordinates) -> Polygon vertex -> Loops vertex units coordinates
init getPosition (Polygon2d { outerLoop, innerLoops }) =
    let
        allLoops =
            (outerLoop :: innerLoops)
                |> List.map (\loop -> removeDuplicates getPosition loop)

        vertexRecords =
            allLoops
                |> List.map (toVertexRecords getPosition)
                |> List.concat

        edges =
            List.foldl
                (\loop ( offset, accumulated ) ->
                    let
                        length =
                            List.length loop

                        newEdges =
                            Array.initialize length
                                (\index ->
                                    if index == 0 then
                                        { startVertexIndex =
                                            offset
                                        , endVertexIndex =
                                            offset + 1
                                        , nextEdgeIndex =
                                            offset + 1
                                        , previousEdgeIndex =
                                            offset + length - 1
                                        }

                                    else if index == length - 1 then
                                        { startVertexIndex =
                                            offset + index
                                        , endVertexIndex =
                                            offset
                                        , nextEdgeIndex =
                                            offset
                                        , previousEdgeIndex =
                                            offset + index - 1
                                        }

                                    else
                                        { startVertexIndex =
                                            offset + index
                                        , endVertexIndex =
                                            offset + index + 1
                                        , nextEdgeIndex =
                                            offset + index + 1
                                        , previousEdgeIndex =
                                            offset + index - 1
                                        }
                                )
                    in
                    ( offset + length, Array.append accumulated newEdges )
                )
                ( 0, Array.empty )
                allLoops
                |> Tuple.second
    in
    { vertexRecords = vertexRecords
    , edges = edges
    }


type alias HelperVertex =
    { index : Int
    , outgoingEdgeIndex : Int
    , isMerge : Bool
    }


type alias State vertex units coordinates =
    { edgeSet : EdgeSet units coordinates
    , helpers : Dict Int HelperVertex -- helper vertex by edge index
    , vertexRecords : Array (VertexRecord vertex units coordinates)
    , edges : Array Edge
    }


getVertexRecord : Int -> State vertex units coordinates -> Maybe (VertexRecord vertex units coordinates)
getVertexRecord index state =
    Array.get index state.vertexRecords


getEdge : Int -> State vertex units coordinates -> Maybe Edge
getEdge index state =
    Array.get index state.edges


error : a -> a
error defaultValue =
    defaultValue


defaultTo : a -> Maybe a -> a
defaultTo defaultValue maybeValue =
    case maybeValue of
        Just actualValue ->
            actualValue

        Nothing ->
            error defaultValue


processLeftEdge : (EdgeSet.Edge units coordinates -> EdgeSet units coordinates -> EdgeSet units coordinates) -> Int -> State vertex units coordinates -> State vertex units coordinates
processLeftEdge insertOrRemove edgeIndex state =
    getEdge edgeIndex state
        |> Maybe.andThen
            (\edge ->
                Maybe.map2
                    (\startVertex endVertex ->
                        let
                            lineSegment =
                                LineSegment2d.fromEndpoints
                                    ( startVertex.position
                                    , endVertex.position
                                    )
                        in
                        { state
                            | edgeSet =
                                state.edgeSet
                                    |> insertOrRemove ( edgeIndex, lineSegment )
                        }
                    )
                    (getVertexRecord edge.startVertexIndex state)
                    (getVertexRecord edge.endVertexIndex state)
            )
        |> defaultTo state


insertLeftEdge : Int -> State vertex units coordinates -> State vertex units coordinates
insertLeftEdge =
    processLeftEdge EdgeSet.insert


removeLeftEdge : Int -> State vertex units coordinates -> State vertex units coordinates
removeLeftEdge =
    processLeftEdge EdgeSet.remove


setHelperOf : Int -> HelperVertex -> State vertex units coordinates -> State vertex units coordinates
setHelperOf edgeIndex helperVertex state =
    { state
        | helpers =
            Dict.insert edgeIndex helperVertex state.helpers
    }


handleStartVertex : Int -> State vertex units coordinates -> State vertex units coordinates
handleStartVertex index state =
    state
        |> insertLeftEdge index
        |> setHelperOf index (HelperVertex index index False)


updateAt : Int -> (a -> a) -> Array a -> Array a
updateAt index function array =
    case Array.get index array of
        Just item ->
            Array.set index (function item) array

        Nothing ->
            array


setPreviousEdge : Int -> Edge -> Edge
setPreviousEdge index edge =
    { edge | previousEdgeIndex = index }


setNextEdge : Int -> Edge -> Edge
setNextEdge index edge =
    { edge | nextEdgeIndex = index }


addDiagonal : Int -> HelperVertex -> State vertex units coordinates -> ( State vertex units coordinates, Int )
addDiagonal vertexIndex helperVertex state =
    let
        n =
            Array.length state.edges
    in
    Maybe.map4
        (\vi vj ei ej ->
            ( { state
                | edges =
                    state.edges
                        |> updateAt vertexIndex (setPreviousEdge (n + 1))
                        |> updateAt helperVertex.outgoingEdgeIndex (setPreviousEdge n)
                        |> updateAt ei.previousEdgeIndex (setNextEdge n)
                        |> updateAt ej.previousEdgeIndex (setNextEdge (n + 1))
                        |> Array.push
                            -- edge index n
                            { startVertexIndex = vertexIndex
                            , endVertexIndex = helperVertex.index
                            , previousEdgeIndex = ei.previousEdgeIndex
                            , nextEdgeIndex = helperVertex.outgoingEdgeIndex
                            }
                        |> Array.push
                            -- edge index n + 1
                            { startVertexIndex = helperVertex.index
                            , endVertexIndex = vertexIndex
                            , previousEdgeIndex = ej.previousEdgeIndex
                            , nextEdgeIndex = vertexIndex
                            }
              }
            , n
            )
        )
        (Array.get vertexIndex state.vertexRecords)
        (Array.get helperVertex.index state.vertexRecords)
        (Array.get vertexIndex state.edges)
        (Array.get helperVertex.outgoingEdgeIndex state.edges)
        |> defaultTo ( state, -1 )


getHelperOf : Int -> State vertex units coordinates -> Maybe HelperVertex
getHelperOf edgeIndex state =
    Dict.get edgeIndex state.helpers


handleEndVertex : Int -> State vertex units coordinates -> State vertex units coordinates
handleEndVertex index state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                diagonalAdded =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex
                                            |> Tuple.first

                                    else
                                        state
                            in
                            diagonalAdded
                                |> removeLeftEdge edge.previousEdgeIndex
                        )
            )
        |> defaultTo state


getLeftEdge : Point2d units coordinates -> State vertex units coordinates -> Maybe Int
getLeftEdge point state =
    EdgeSet.leftOf point state.edgeSet


handleSplitVertex : Int -> Point2d units coordinates -> State vertex units coordinates -> State vertex units coordinates
handleSplitVertex index point state =
    getLeftEdge point state
        |> Maybe.andThen
            (\edgeIndex ->
                getHelperOf edgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                ( updatedState, outgoingEdgeIndex ) =
                                    addDiagonal index helperVertex state
                            in
                            updatedState
                                |> setHelperOf edgeIndex (HelperVertex index outgoingEdgeIndex False)
                                |> insertLeftEdge index
                                |> setHelperOf index (HelperVertex index index False)
                        )
            )
        |> defaultTo state


handleMergeVertex : Int -> Point2d units coordinates -> State vertex units coordinates -> State vertex units coordinates
handleMergeVertex index point state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.andThen
                        (\rightHelper ->
                            let
                                rightDiagonalAdded =
                                    if rightHelper.isMerge then
                                        state
                                            |> addDiagonal index rightHelper
                                            |> Tuple.first

                                    else
                                        state

                                rightUpdated =
                                    rightDiagonalAdded
                                        |> removeLeftEdge edge.previousEdgeIndex
                            in
                            getLeftEdge point rightUpdated
                                |> Maybe.andThen
                                    (\leftEdgeIndex ->
                                        getHelperOf leftEdgeIndex state
                                            |> Maybe.map
                                                (\leftHelper ->
                                                    let
                                                        ( leftDiagonalAdded, leftOutgoing ) =
                                                            if leftHelper.isMerge then
                                                                rightUpdated
                                                                    |> addDiagonal index leftHelper

                                                            else
                                                                ( rightUpdated, index )
                                                    in
                                                    leftDiagonalAdded
                                                        |> setHelperOf leftEdgeIndex (HelperVertex index leftOutgoing True)
                                                )
                                    )
                        )
            )
        |> defaultTo state


handleRightVertex : Int -> Point2d units coordinates -> State vertex units coordinates -> State vertex units coordinates
handleRightVertex index point state =
    getLeftEdge point state
        |> Maybe.andThen
            (\leftEdgeIndex ->
                getHelperOf leftEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                ( diagonalAdded, outgoingEdgeIndex ) =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex

                                    else
                                        ( state, index )
                            in
                            diagonalAdded
                                |> setHelperOf leftEdgeIndex (HelperVertex index outgoingEdgeIndex False)
                        )
            )
        |> defaultTo state


handleLeftVertex : Int -> State vertex units coordinates -> State vertex units coordinates
handleLeftVertex index state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                diagonalAdded =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex
                                            |> Tuple.first

                                    else
                                        state
                            in
                            diagonalAdded
                                |> removeLeftEdge edge.previousEdgeIndex
                                |> insertLeftEdge index
                                |> setHelperOf index (HelperVertex index index False)
                        )
            )
        |> defaultTo state


type alias MonotoneVertexRecord vertex units coordinates =
    { index : Int
    , vertex : vertex
    , position : Point2d units coordinates
    , nextVertexIndex : Int
    }


buildLoop : State vertex units coordinates -> Array (VertexRecord vertex units coordinates) -> Int -> Int -> ( Set Int, List (MonotoneVertexRecord vertex units coordinates) ) -> ( Set Int, List (MonotoneVertexRecord vertex units coordinates) )
buildLoop state vertexRecordArray startIndex currentIndex ( processedEdgeIndices, accumulated ) =
    case getEdge currentIndex state of
        Just currentEdge ->
            case Array.get currentEdge.startVertexIndex vertexRecordArray of
                Just vertexRecord ->
                    let
                        updatedEdgeIndices =
                            Set.insert currentIndex processedEdgeIndices

                        newMonotoneVertexRecord =
                            { index = currentEdge.startVertexIndex
                            , vertex = vertexRecord.vertex
                            , position = vertexRecord.position
                            , nextVertexIndex = currentEdge.endVertexIndex
                            }

                        newAccumulated =
                            newMonotoneVertexRecord :: accumulated

                        nextIndex =
                            currentEdge.nextEdgeIndex
                    in
                    if nextIndex == startIndex then
                        ( updatedEdgeIndices
                        , List.reverse newAccumulated
                        )

                    else
                        buildLoop
                            state
                            vertexRecordArray
                            startIndex
                            nextIndex
                            ( updatedEdgeIndices, newAccumulated )

                Nothing ->
                    error ( processedEdgeIndices, [] )

        Nothing ->
            error ( processedEdgeIndices, [] )


collectMonotoneLoops : State vertex units coordinates -> ( Array vertex, List (List (MonotoneVertexRecord vertex units coordinates)) )
collectMonotoneLoops state =
    let
        vertices =
            state.vertexRecords |> Array.map .vertex

        processStartEdge index accumulated =
            let
                ( processedEdgeIndices, accumulatedLoops ) =
                    accumulated
            in
            if Set.member index processedEdgeIndices then
                accumulated

            else
                let
                    ( updatedEdgeIndices, loop ) =
                        buildLoop state state.vertexRecords index index ( processedEdgeIndices, [] )
                in
                ( updatedEdgeIndices, loop :: accumulatedLoops )

        allEdgeIndices =
            List.range 0 (Array.length state.edges - 1)

        ( _, loops ) =
            List.foldl processStartEdge ( Set.empty, [] ) allEdgeIndices
    in
    ( vertices, loops )


monotonePolygons : (vertex -> Point2d units coordinates) -> Polygon vertex -> ( Array vertex, List (List (MonotoneVertexRecord vertex units coordinates)) )
monotonePolygons getPosition polygon =
    let
        { vertexRecords, edges } =
            init getPosition polygon

        priorityQueue =
            vertexRecords
                |> List.indexedMap Tuple.pair
                |> List.sortWith
                    (\( _, firstVertexRecord ) ( _, secondVertexRecord ) ->
                        comparePoints secondVertexRecord.position firstVertexRecord.position
                    )

        vertexRecordArray =
            Array.fromList vertexRecords

        initialState =
            { edgeSet = EdgeSet.empty
            , helpers = Dict.empty
            , vertexRecords = vertexRecordArray
            , edges = edges
            }

        handleVertex ( index, vertex ) current =
            case vertex.kind of
                Start ->
                    handleStartVertex index current

                End ->
                    handleEndVertex index current

                Right ->
                    handleRightVertex index vertex.position current

                Left ->
                    handleLeftVertex index current

                Split ->
                    handleSplitVertex index vertex.position current

                Merge ->
                    handleMergeVertex index vertex.position current

        finalState =
            List.foldl handleVertex initialState priorityQueue
    in
    collectMonotoneLoops finalState


type alias TriangulationState vertex units coordinates =
    { chainStart : MonotoneVertexRecord vertex units coordinates
    , chainInterior : List (MonotoneVertexRecord vertex units coordinates)
    , chainEnd : MonotoneVertexRecord vertex units coordinates
    , faces : List ( Int, Int, Int )
    }


signedArea : MonotoneVertexRecord vertex units coordinates -> MonotoneVertexRecord vertex units coordinates -> MonotoneVertexRecord vertex units coordinates -> Quantity Float (Squared units)
signedArea first second third =
    Triangle2d.counterclockwiseArea <|
        Triangle2d.from first.position second.position third.position


addLeftChainVertex : MonotoneVertexRecord vertex units coordinates -> TriangulationState vertex units coordinates -> TriangulationState vertex units coordinates
addLeftChainVertex vertex state =
    case state.chainInterior of
        [] ->
            if
                signedArea state.chainStart state.chainEnd vertex
                    |> Quantity.greaterThan Quantity.zero
            then
                let
                    newFace =
                        ( state.chainStart.index
                        , state.chainEnd.index
                        , vertex.index
                        )
                in
                { chainStart = state.chainStart
                , chainInterior = []
                , chainEnd = vertex
                , faces = newFace :: state.faces
                }

            else
                { chainStart = state.chainStart
                , chainInterior = [ state.chainEnd ]
                , chainEnd = vertex
                , faces = state.faces
                }

        firstInterior :: restInterior ->
            if
                signedArea firstInterior state.chainEnd vertex
                    |> Quantity.greaterThan Quantity.zero
            then
                let
                    newFace =
                        ( firstInterior.index
                        , state.chainEnd.index
                        , vertex.index
                        )
                in
                addLeftChainVertex vertex
                    { chainStart = state.chainStart
                    , chainInterior = restInterior
                    , chainEnd = firstInterior
                    , faces = newFace :: state.faces
                    }

            else
                { chainStart = state.chainStart
                , chainInterior = state.chainEnd :: state.chainInterior
                , chainEnd = vertex
                , faces = state.faces
                }


addRightChainVertex : MonotoneVertexRecord vertex units coordinates -> TriangulationState vertex units coordinates -> TriangulationState vertex units coordinates
addRightChainVertex vertex state =
    case state.chainInterior of
        [] ->
            if
                signedArea vertex state.chainEnd state.chainStart
                    |> Quantity.greaterThan Quantity.zero
            then
                let
                    newFace =
                        ( vertex.index
                        , state.chainEnd.index
                        , state.chainStart.index
                        )
                in
                { chainStart = state.chainStart
                , chainInterior = []
                , chainEnd = vertex
                , faces = newFace :: state.faces
                }

            else
                { chainStart = state.chainStart
                , chainInterior = [ state.chainEnd ]
                , chainEnd = vertex
                , faces = state.faces
                }

        firstInterior :: restInterior ->
            if
                signedArea vertex state.chainEnd firstInterior
                    |> Quantity.greaterThan Quantity.zero
            then
                let
                    newFace =
                        ( vertex.index
                        , state.chainEnd.index
                        , firstInterior.index
                        )
                in
                addRightChainVertex vertex
                    { chainStart = state.chainStart
                    , chainInterior = restInterior
                    , chainEnd = firstInterior
                    , faces = newFace :: state.faces
                    }

            else
                { chainStart = state.chainStart
                , chainInterior = state.chainEnd :: state.chainInterior
                , chainEnd = vertex
                , faces = state.faces
                }


startNewRightChain : MonotoneVertexRecord vertex units coordinates -> TriangulationState vertex units coordinates -> TriangulationState vertex units coordinates
startNewRightChain vertex state =
    let
        collectFaces firstVertex otherVertices accumulated =
            case otherVertices of
                [] ->
                    let
                        newFace =
                            ( firstVertex.index
                            , vertex.index
                            , state.chainStart.index
                            )
                    in
                    newFace :: accumulated

                firstOther :: restOther ->
                    let
                        newFace =
                            ( firstVertex.index
                            , vertex.index
                            , firstOther.index
                            )
                    in
                    collectFaces firstOther restOther (newFace :: accumulated)
    in
    { chainStart = state.chainEnd
    , chainInterior = []
    , chainEnd = vertex
    , faces = collectFaces state.chainEnd state.chainInterior state.faces
    }


startNewLeftChain : MonotoneVertexRecord vertex units coordinates -> TriangulationState vertex units coordinates -> TriangulationState vertex units coordinates
startNewLeftChain vertex state =
    let
        collectFaces firstVertex otherVertices accumulated =
            case otherVertices of
                [] ->
                    let
                        newFace =
                            ( vertex.index
                            , firstVertex.index
                            , state.chainStart.index
                            )
                    in
                    newFace :: accumulated

                firstOther :: restOther ->
                    let
                        newFace =
                            ( vertex.index
                            , firstVertex.index
                            , firstOther.index
                            )
                    in
                    collectFaces firstOther restOther (newFace :: accumulated)
    in
    { chainStart = state.chainEnd
    , chainInterior = []
    , chainEnd = vertex
    , faces = collectFaces state.chainEnd state.chainInterior state.faces
    }


faces : List (MonotoneVertexRecord vertex units coordinates) -> List ( Int, Int, Int )
faces vertices =
    let
        sortedVertices =
            vertices
                |> List.sortWith
                    (\first second ->
                        comparePoints second.position first.position
                    )
    in
    case sortedVertices of
        [] ->
            []

        [ single ] ->
            []

        first :: second :: rest ->
            let
                initialState =
                    { chainStart = first
                    , chainInterior = []
                    , chainEnd = second
                    , faces = []
                    }

                processVertex vertex state =
                    if vertex.nextVertexIndex == state.chainStart.index then
                        -- new vertex on the right will connect to all chain
                        -- vertices on the left
                        startNewRightChain vertex state

                    else if state.chainStart.nextVertexIndex == vertex.index then
                        -- new vertex on the left will connect to all chain
                        -- vertices on the right
                        startNewLeftChain vertex state

                    else if vertex.nextVertexIndex == state.chainEnd.index then
                        -- continuing left chain
                        addRightChainVertex vertex state

                    else if state.chainEnd.nextVertexIndex == vertex.index then
                        -- continuing right chain
                        addLeftChainVertex vertex state

                    else
                        error state
            in
            List.foldl processVertex initialState rest |> .faces


triangulation : (vertex -> Point2d units coordinates) -> Polygon vertex -> TriangularMesh vertex
triangulation getPosition polygon =
    let
        ( points, loops ) =
            monotonePolygons getPosition polygon
    in
    TriangularMesh.indexed points (List.map faces loops |> List.concat)
