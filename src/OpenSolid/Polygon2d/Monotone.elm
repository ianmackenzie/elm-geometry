module OpenSolid.Polygon2d.Monotone
    exposing
        ( init
        , polygons
        , testPolygon
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import OpenSolid.Geometry.Internal as Internal exposing (Polygon2d(..))
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d.EdgeSet as EdgeSet exposing (EdgeSet)
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import Set


type Kind
    = Start
    | End
    | Left
    | Right
    | Split
    | Merge


type alias Vertex =
    { position : Point2d
    , kind : Kind
    }


comparePoints : Point2d -> Point2d -> Order
comparePoints p1 p2 =
    let
        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2
    in
    if y1 < y2 then
        LT
    else if y1 > y2 then
        GT
    else
        compare x2 x1


leftTurn : Point2d -> Point2d -> Point2d -> Bool
leftTurn p1 p2 p3 =
    let
        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3
    in
    (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2) > 0


kind : Point2d -> Point2d -> Point2d -> Kind
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


toVertices : List Point2d -> List Vertex
toVertices points =
    case points of
        [] ->
            []

        first :: rest ->
            let
                last =
                    List.foldl always first rest

                collect previous points accumulated =
                    case points of
                        [] ->
                            []

                        [ last ] ->
                            let
                                newVertex =
                                    { position = last
                                    , kind = kind previous last first
                                    }
                            in
                            List.reverse (newVertex :: accumulated)

                        current :: next :: rest ->
                            let
                                newVertex =
                                    { position = current
                                    , kind = kind previous current next
                                    }
                            in
                            collect
                                current
                                (next :: rest)
                                (newVertex :: accumulated)
            in
            collect last points []


type alias Edge =
    { startVertexIndex : Int
    , endVertexIndex : Int
    , nextEdgeIndex : Int
    , previousEdgeIndex : Int
    }


type alias Loops =
    { vertices : List Vertex
    , edges : Array Edge
    }


removeDuplicates : List Point2d -> List Point2d -> List Point2d
removeDuplicates points accumulatedPoints =
    case points of
        [] ->
            List.reverse accumulatedPoints

        firstPoint :: remainingPoints ->
            case accumulatedPoints of
                [] ->
                    removeDuplicates remainingPoints [ firstPoint ]

                firstAccumulatedPoint :: _ ->
                    if firstPoint == firstAccumulatedPoint then
                        removeDuplicates remainingPoints accumulatedPoints
                    else
                        removeDuplicates remainingPoints
                            (firstPoint :: accumulatedPoints)


init : Polygon2d -> Loops
init (Polygon2d { outerLoop, innerLoops }) =
    let
        allLoops =
            (outerLoop :: innerLoops)
                |> List.map (\loop -> removeDuplicates loop [])

        vertices =
            allLoops
                |> List.map toVertices
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
                                    { startVertexIndex =
                                        index + offset
                                    , endVertexIndex =
                                        ((index + 1) % length) + offset
                                    , nextEdgeIndex =
                                        ((index + 1) % length) + offset
                                    , previousEdgeIndex =
                                        ((index - 1) % length) + offset
                                    }
                                )
                    in
                    ( offset + length, Array.append accumulated newEdges )
                )
                ( 0, Array.empty )
                allLoops
                |> Tuple.second
    in
    { vertices = vertices
    , edges = edges
    }


type alias HelperVertex =
    { index : Int
    , outgoingEdgeIndex : Int
    , isMerge : Bool
    }


type alias State =
    { edgeSet : EdgeSet
    , helpers : Dict Int HelperVertex -- helper vertex by edge index
    , vertices : Array Vertex
    , edges : Array Edge
    }


getVertex : Int -> State -> Maybe Vertex
getVertex index state =
    Array.get index state.vertices


getEdge : Int -> State -> Maybe Edge
getEdge index state =
    Array.get index state.edges


error : a -> a
error defaultValue =
    Debug.crash "Unexpected"


defaultTo : a -> Maybe a -> a
defaultTo defaultValue maybeValue =
    case maybeValue of
        Just actualValue ->
            actualValue

        Nothing ->
            error defaultValue


processLeftEdge : (EdgeSet.Edge -> EdgeSet -> EdgeSet) -> Int -> State -> State
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
                    (getVertex edge.startVertexIndex state)
                    (getVertex edge.endVertexIndex state)
            )
        |> defaultTo state


insertLeftEdge : Int -> State -> State
insertLeftEdge =
    processLeftEdge EdgeSet.insert


removeLeftEdge : Int -> State -> State
removeLeftEdge =
    processLeftEdge EdgeSet.remove


setHelperOf : Int -> HelperVertex -> State -> State
setHelperOf edgeIndex helperVertex state =
    { state
        | helpers =
            Dict.insert edgeIndex helperVertex state.helpers
    }


handleStartVertex : Int -> State -> State
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


addDiagonal : Int -> HelperVertex -> State -> ( State, Int )
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
        (Array.get vertexIndex state.vertices)
        (Array.get helperVertex.index state.vertices)
        (Array.get vertexIndex state.edges)
        (Array.get helperVertex.outgoingEdgeIndex state.edges)
        |> defaultTo ( state, -1 )


getHelperOf : Int -> State -> Maybe HelperVertex
getHelperOf edgeIndex state =
    Dict.get edgeIndex state.helpers


handleEndVertex : Int -> State -> State
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


getLeftEdge : Point2d -> State -> Maybe Int
getLeftEdge point state =
    EdgeSet.leftOf point state.edgeSet


handleSplitVertex : Int -> Point2d -> State -> State
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


handleMergeVertex : Int -> Point2d -> State -> State
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


handleRightVertex : Int -> Point2d -> State -> State
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


handleLeftVertex : Int -> State -> State
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


type alias MonotoneVertex =
    { index : Int
    , position : Point2d
    , nextVertexIndex : Int
    }


collectMonotoneLoops : State -> ( Array Point2d, List (List MonotoneVertex) )
collectMonotoneLoops state =
    let
        points =
            state.vertices |> Array.map .position

        buildLoop startIndex currentIndex ( processedEdgeIndices, accumulated ) =
            case getEdge currentIndex state of
                Just currentEdge ->
                    case Array.get currentEdge.startVertexIndex points of
                        Just vertexPosition ->
                            let
                                updatedEdgeIndices =
                                    Set.insert currentIndex processedEdgeIndices

                                newVertex =
                                    { index = currentEdge.startVertexIndex
                                    , position = vertexPosition
                                    , nextVertexIndex = currentEdge.endVertexIndex
                                    }

                                newAccumulated =
                                    newVertex :: accumulated

                                nextIndex =
                                    currentEdge.nextEdgeIndex
                            in
                            if nextIndex == startIndex then
                                ( updatedEdgeIndices
                                , List.reverse newAccumulated
                                )
                            else
                                buildLoop
                                    startIndex
                                    nextIndex
                                    ( updatedEdgeIndices, newAccumulated )

                        Nothing ->
                            error ( processedEdgeIndices, [] )

                Nothing ->
                    error ( processedEdgeIndices, [] )

        processStartEdge index accumulated =
            let
                ( processedEdgeIndices, loops ) =
                    accumulated
            in
            if Set.member index processedEdgeIndices then
                accumulated
            else
                let
                    ( updatedEdgeIndices, loop ) =
                        buildLoop index index ( processedEdgeIndices, [] )
                in
                ( updatedEdgeIndices, loop :: loops )

        allEdgeIndices =
            List.range 0 (Array.length state.edges - 1)

        ( _, loops ) =
            List.foldl processStartEdge ( Set.empty, [] ) allEdgeIndices
    in
    ( points, loops )


testPolygon : Polygon2d
testPolygon =
    let
        loop =
            List.map Point2d.fromCoordinates

        outerLoop =
            loop [ ( 0, 0 ), ( 5, 0 ), ( 5, 3 ), ( 0, 3 ) ]

        innerLoops =
            [ loop [ ( 1, 2 ), ( 2, 2 ), ( 2, 1 ), ( 1, 1 ) ]
            , loop [ ( 3, 2 ), ( 4, 1.5 ), ( 3, 1 ) ]
            ]
    in
    Polygon2d { outerLoop = outerLoop, innerLoops = innerLoops }


polygons : Polygon2d -> ( Array Point2d, List (List MonotoneVertex) )
polygons polygon =
    let
        { vertices, edges } =
            init polygon

        priorityQueue =
            vertices
                |> List.indexedMap (,)
                |> List.sortWith
                    (\( _, firstVertex ) ( _, secondVertex ) ->
                        comparePoints secondVertex.position firstVertex.position
                    )

        initialState =
            { edgeSet = EdgeSet.empty
            , helpers = Dict.empty
            , vertices = Array.fromList vertices
            , edges = edges
            }

        vertexArray =
            Array.fromList vertices

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


type alias TriangulationState =
    { chainStart : MonotoneVertex
    , chainInterior : List MonotoneVertex
    , chainEnd : MonotoneVertex
    , faces : List ( Int, Int, Int )
    }


positiveArea : MonotoneVertex -> MonotoneVertex -> MonotoneVertex -> Bool
positiveArea first second third =
    let
        triangle =
            Triangle2d.fromVertices
                ( first.position
                , second.position
                , third.position
                )
    in
    Triangle2d.counterclockwiseArea triangle > 0


addChainVertex : MonotoneVertex -> TriangulationState -> TriangulationState
addChainVertex vertex state =
    case state.chainInterior of
        [] ->
            state

        first :: rest ->
            state


faces : List MonotoneVertex -> List ( Int, Int, Int )
faces vertices =
    let
        sortedVertices =
            vertices
                |> List.sortWith
                    (\first second ->
                        comparePoints first.position second.position
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
                    let
                        { chainStart, chainInterior, chainEnd, faces } =
                            state

                        continuingRight =
                            chainEnd.nextVertexIndex == vertex.index

                        continuingLeft =
                            vertex.nextVertexIndex == chainEnd.index
                    in
                    if continuingLeft || continuingRight then
                        -- continuing chain
                        state
                    else
                        -- on other side
                        state
            in
            List.foldl processVertex initialState vertices
                |> .faces
