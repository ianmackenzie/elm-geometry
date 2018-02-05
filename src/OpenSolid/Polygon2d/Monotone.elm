module OpenSolid.Polygon2d.Monotone
    exposing
        ( init
        , polygons
        , testPolygon
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polygon2d.EdgeSet as EdgeSet exposing (EdgeSet)
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
init (Internal.Polygon2d { outerLoop, innerLoops }) =
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


type alias State =
    { edgeSet : EdgeSet
    , helpers : Dict Int Int -- helper vertex index by edge index
    , outgoingEdges : Dict Int Int -- outgoing edge index by vertex index
    , vertices : Array Vertex
    , edges : Array Edge
    }


if_ : Bool -> (State -> State) -> State -> State
if_ condition operation state =
    if condition then
        operation state
    else
        state


getVertex : Int -> State -> Maybe Vertex
getVertex index state =
    Array.get index state.vertices


getEdge : Int -> State -> Maybe Edge
getEdge index state =
    Array.get index state.edges


defaultTo : a -> Maybe a -> a
defaultTo defaultValue maybeValue =
    case maybeValue of
        Just actualValue ->
            actualValue

        Nothing ->
            Debug.crash "Unexpected"


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


setHelperOf : Int -> Int -> State -> State
setHelperOf edgeIndex vertexIndex state =
    { state
        | helpers =
            Dict.insert edgeIndex vertexIndex state.helpers
    }


handleStartVertex : Int -> State -> State
handleStartVertex index state =
    state
        |> insertLeftEdge index
        |> setHelperOf index index


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


outgoingEdgeIndex : Int -> State -> Int
outgoingEdgeIndex vertexIndex state =
    Dict.get vertexIndex state.outgoingEdges |> Maybe.withDefault vertexIndex


addDiagonal : Int -> Int -> Bool -> State -> State
addDiagonal i j replaceOutgoing state =
    let
        n =
            Array.length state.edges

        outgoingEdgeIndexI =
            outgoingEdgeIndex i state

        outgoingEdgeIndexJ =
            outgoingEdgeIndex j state
    in
    Maybe.map4
        (\vi vj ei ej ->
            { state
                | edges =
                    state.edges
                        |> updateAt outgoingEdgeIndexI (setPreviousEdge (n + 1))
                        |> updateAt outgoingEdgeIndexJ (setPreviousEdge n)
                        |> updateAt ei.previousEdgeIndex (setNextEdge n)
                        |> updateAt ej.previousEdgeIndex (setNextEdge (n + 1))
                        |> Array.push
                            -- edge index n
                            { startVertexIndex = i
                            , endVertexIndex = j
                            , previousEdgeIndex = ei.previousEdgeIndex
                            , nextEdgeIndex = outgoingEdgeIndexJ
                            }
                        |> Array.push
                            -- edge index n + 1
                            { startVertexIndex = j
                            , endVertexIndex = i
                            , previousEdgeIndex = ej.previousEdgeIndex
                            , nextEdgeIndex = outgoingEdgeIndexI
                            }
                , outgoingEdges =
                    if replaceOutgoing then
                        Dict.insert i n state.outgoingEdges
                    else
                        state.outgoingEdges
            }
        )
        (Array.get i state.vertices)
        (Array.get j state.vertices)
        (Array.get outgoingEdgeIndexI state.edges)
        (Array.get outgoingEdgeIndexJ state.edges)
        |> defaultTo state


vertexIsMerge : Int -> State -> Bool
vertexIsMerge vertexIndex state =
    getVertex vertexIndex state
        |> Maybe.map (\vertex -> vertex.kind == Merge)
        |> Maybe.withDefault False


getHelperOf : Int -> State -> Maybe Int
getHelperOf edgeIndex state =
    Dict.get edgeIndex state.helpers


handleEndVertex : Int -> State -> State
handleEndVertex index state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.map
                        (\helperVertexIndex ->
                            state
                                |> if_ (vertexIsMerge helperVertexIndex state)
                                    (addDiagonal index helperVertexIndex False)
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
                        (\helperVertexIndex ->
                            state
                                |> addDiagonal index helperVertexIndex False
                                |> setHelperOf edgeIndex index
                                |> insertLeftEdge index
                                |> setHelperOf index index
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
                        (\previousEdgeHelper ->
                            let
                                updatedState =
                                    state
                                        |> if_ (vertexIsMerge previousEdgeHelper state)
                                            (addDiagonal index previousEdgeHelper False)
                                        |> removeLeftEdge edge.previousEdgeIndex
                            in
                            getLeftEdge point updatedState
                                |> Maybe.andThen
                                    (\leftEdgeIndex ->
                                        getHelperOf leftEdgeIndex state
                                            |> Maybe.map
                                                (\leftEdgeHelper ->
                                                    updatedState
                                                        |> if_ (vertexIsMerge leftEdgeHelper updatedState)
                                                            (addDiagonal index leftEdgeHelper True)
                                                        |> setHelperOf leftEdgeIndex index
                                                )
                                    )
                        )
            )
        |> defaultTo state



--handleMergeVertex : Int -> Point2d -> State -> State
--handleMergeVertex index point state =
--    Maybe.map2 (,) (getEdge index state) (getLeftEdge point state)
--        |> Maybe.andThen
--            (\( edge, leftEdgeIndex ) ->
--                Maybe.map2 (,)
--                    (getHelperOf edge.previousEdgeIndex state)
--                    (getHelperOf leftEdgeIndex state)
--                    |> Maybe.map
--                        (\( previousEdgeHelper, leftEdgeHelper ) ->
--                            state
--                                |> if_ (vertexIsMerge previousEdgeHelper state)
--                                    (addDiagonal index previousEdgeHelper)
--                                |> removeLeftEdge edge.previousEdgeIndex
--                                |> if_ (vertexIsMerge leftEdgeHelper state)
--                                    (addDiagonal index leftEdgeHelper)
--                                |> setHelperOf leftEdgeIndex index
--                        )
--            )
--        |> fallBackTo state


handleRightVertex : Int -> Point2d -> State -> State
handleRightVertex index point state =
    getLeftEdge point state
        |> Maybe.andThen
            (\leftEdgeIndex ->
                getHelperOf leftEdgeIndex state
                    |> Maybe.map
                        (\helperVertexIndex ->
                            state
                                |> if_ (vertexIsMerge helperVertexIndex state)
                                    (addDiagonal index helperVertexIndex True)
                                |> setHelperOf leftEdgeIndex index
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
                        (\helperVertexIndex ->
                            state
                                |> if_ (vertexIsMerge helperVertexIndex state)
                                    (addDiagonal index helperVertexIndex False)
                                |> removeLeftEdge edge.previousEdgeIndex
                                |> insertLeftEdge index
                                |> setHelperOf index index
                        )
            )
        |> defaultTo state


collectMonotoneLoops : State -> ( Array Point2d, List (Array Int) )
collectMonotoneLoops state =
    let
        points =
            state.vertices |> Array.map .position

        buildLoop startIndex currentIndex ( processedEdgeIndices, accumulated ) =
            getEdge currentIndex state
                |> Maybe.map
                    (\currentEdge ->
                        let
                            updatedEdgeIndices =
                                Set.insert currentIndex processedEdgeIndices

                            newAccumulated =
                                currentEdge.startVertexIndex :: accumulated

                            nextIndex =
                                currentEdge.nextEdgeIndex
                        in
                        if nextIndex == startIndex then
                            ( updatedEdgeIndices
                            , Array.fromList (List.reverse newAccumulated)
                            )
                        else
                            buildLoop
                                startIndex
                                nextIndex
                                ( updatedEdgeIndices, newAccumulated )
                    )
                |> defaultTo ( processedEdgeIndices, Array.empty )

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
    Polygon2d.withHoles outerLoop innerLoops


polygons : Polygon2d -> ( Array Point2d, List (Array Int) )
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
            , outgoingEdges = Dict.empty
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

        _ =
            finalState.edges
                |> Array.toList
                |> List.map (\edge -> ( edge.previousEdgeIndex, ( edge.startVertexIndex, edge.endVertexIndex ), edge.nextEdgeIndex ))
                |> Debug.log "edges"
    in
    collectMonotoneLoops finalState
