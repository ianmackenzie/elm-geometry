module OpenSolid.Polygon2d.Monotone
    exposing
        ( init
        , polygons
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import OpenSolid.Geometry.Internal as Internal exposing (Polygon2d(..))
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d.EdgeSet as EdgeSet exposing (EdgeSet)


type Kind
    = Start
    | End
    | Regular
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
    else
        Regular


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


init : Polygon2d -> Loops
init (Polygon2d { outerLoop, innerLoops }) =
    let
        allLoops =
            outerLoop :: innerLoops

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
    , helpers : Dict Int Int
    , vertices : Array Vertex
    , edges : Array Edge
    }


processLeftEdge : (EdgeSet.Edge -> EdgeSet -> EdgeSet) -> Int -> State -> State
processLeftEdge insertOrRemove edgeIndex state =
    Array.get edgeIndex state.edges
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
                    (Array.get edge.startVertexIndex state.vertices)
                    (Array.get edge.endVertexIndex state.vertices)
            )
        |> Maybe.withDefault state


insertLeftEdge : Int -> State -> State
insertLeftEdge =
    processLeftEdge EdgeSet.insert


removeLeftEdge : Int -> State -> State
removeLeftEdge =
    processLeftEdge EdgeSet.remove


setHelper : Int -> Int -> State -> State
setHelper edgeIndex vertexIndex state =
    { state
        | helpers =
            Dict.insert edgeIndex vertexIndex state.helpers
    }


handleStartVertex : Int -> State -> State
handleStartVertex index state =
    state
        |> insertLeftEdge index
        |> setHelper index index


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


addDiagonal : Int -> Int -> State -> State
addDiagonal i j state =
    let
        n =
            Array.length state.edges
    in
    Maybe.map4
        (\vi vj ei ej ->
            { state
                | edges =
                    state.edges
                        |> updateAt i (setPreviousEdge (n + 1))
                        |> updateAt j (setPreviousEdge n)
                        |> updateAt ei.previousEdgeIndex (setNextEdge n)
                        |> updateAt ej.previousEdgeIndex (setNextEdge (n + 1))
                        |> Array.push
                            { startVertexIndex = i
                            , endVertexIndex = j
                            , previousEdgeIndex = ei.previousEdgeIndex
                            , nextEdgeIndex = j
                            }
                        |> Array.push
                            { startVertexIndex = j
                            , endVertexIndex = i
                            , previousEdgeIndex = ej.previousEdgeIndex
                            , nextEdgeIndex = i
                            }
            }
        )
        (Array.get i state.vertices)
        (Array.get j state.vertices)
        (Array.get i state.edges)
        (Array.get j state.edges)
        |> Maybe.withDefault state


vertexIsMerge : Int -> State -> Bool
vertexIsMerge vertexIndex state =
    case Array.get vertexIndex state.vertices of
        Just vertex ->
            vertex.kind == Merge

        Nothing ->
            False


if_ : Bool -> (a -> a) -> a -> a
if_ condition function argument =
    if condition then
        function argument
    else
        argument


handleEndVertex : Int -> State -> State
handleEndVertex index state =
    case Array.get index state.edges of
        Just edge ->
            case Dict.get edge.previousEdgeIndex state.helpers of
                Just helperVertexIndex ->
                    state
                        |> if_ (vertexIsMerge helperVertexIndex state)
                            (addDiagonal index helperVertexIndex)
                        |> removeLeftEdge edge.previousEdgeIndex

                Nothing ->
                    state

        Nothing ->
            state


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

                Regular ->
                    current

                Split ->
                    current

                Merge ->
                    current

        finalState =
            List.foldl handleVertex initialState priorityQueue
    in
    ( Array.empty, [] )
