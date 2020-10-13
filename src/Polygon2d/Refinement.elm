module Polygon2d.Refinement exposing (refine)

import Array exposing (Array)
import Dict exposing (Dict)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d, coordinates)
import Quantity exposing (Quantity)
import TriangularMesh exposing (TriangularMesh)


type alias Vertex units coordinates =
    { position : Point2d units coordinates
    , index : Int
    }


type alias ShortEdge units coordinates =
    { start : Vertex units coordinates
    , end : Vertex units coordinates
    }


type alias LongEdge units coordinates =
    { start : Vertex units coordinates
    , firstEdge : Edge units coordinates
    , secondEdge : Edge units coordinates
    , end : Vertex units coordinates
    , level : Int
    }


type Edge units coordinates
    = Short (ShortEdge units coordinates)
    | Long (LongEdge units coordinates)


type alias State units coordinates =
    { vertexStack : List (Point2d units coordinates)
    , vertexCount : Int
    , edgeDict : Dict ( Int, Int ) (Edge units coordinates)
    , accumulatedFaces : List ( Int, Int, Int )
    }


type alias Face units coordinates =
    ( Edge units coordinates, Edge units coordinates, Edge units coordinates )


type alias SubdivisionFunction units coordinates =
    Point2d units coordinates -> Point2d units coordinates -> Int


initState : TriangularMesh (Point2d units coordinates) -> State units coordinates
initState triangularMesh =
    let
        vertexArray =
            TriangularMesh.vertices triangularMesh
    in
    { vertexStack = Array.foldl (::) [] vertexArray
    , vertexCount = Array.length vertexArray
    , edgeDict = Dict.empty
    , accumulatedFaces = []
    }


addVertex :
    Point2d units coordinates
    -> State units coordinates
    -> ( Vertex units coordinates, State units coordinates )
addVertex position state =
    let
        currentCount =
            state.vertexCount
    in
    ( { position = position
      , index = currentCount
      }
    , { vertexStack = position :: state.vertexStack
      , vertexCount = currentCount + 1
      , edgeDict = state.edgeDict
      , accumulatedFaces = state.accumulatedFaces
      }
    )


reverseEdge : Edge units coordinates -> Edge units coordinates
reverseEdge edge =
    case edge of
        Short { start, end } ->
            Short { start = end, end = start }

        Long { start, firstEdge, secondEdge, end, level } ->
            Long
                { start = end
                , firstEdge = reverseEdge secondEdge
                , secondEdge = reverseEdge firstEdge
                , end = start
                , level = level
                }


buildEdge :
    Int
    -> Vertex units coordinates
    -> Vertex units coordinates
    -> State units coordinates
    -> ( Edge units coordinates, State units coordinates )
buildEdge level startVertex endVertex state0 =
    if level <= 0 then
        ( Short { start = startVertex, end = endVertex }, state0 )

    else
        let
            midpoint =
                Point2d.midpoint startVertex.position endVertex.position

            ( midVertex, state1 ) =
                addVertex midpoint state0

            ( firstEdge, state2 ) =
                buildEdge (level - 1) startVertex midVertex state1

            ( secondEdge, state3 ) =
                buildEdge (level - 1) midVertex endVertex state2
        in
        ( Long
            { start = startVertex
            , firstEdge = firstEdge
            , secondEdge = secondEdge
            , end = endVertex
            , level = level
            }
        , state3
        )


subdivisionLevels :
    SubdivisionFunction units coordinates
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> Int
subdivisionLevels subdivisionFunction startPoint endPoint =
    let
        desiredSubdivisions =
            subdivisionFunction startPoint endPoint

        computedLevels =
            ceiling (logBase 2 (toFloat desiredSubdivisions))
    in
    if desiredSubdivisions == 2 ^ (computedLevels - 1) then
        -- Handle the case where desiredSubdivisions is already an exact power
        -- of two and roundoff causes computedLevels to be one higher than it
        -- should be
        computedLevels - 1

    else
        computedLevels


createEdge :
    TriangularMesh (Point2d units coordinates)
    -> SubdivisionFunction units coordinates
    -> Int
    -> Int
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> State units coordinates
    -> ( Edge units coordinates, State units coordinates )
createEdge triangularMesh subdivisionFunction i j startPoint endPoint state0 =
    let
        startVertex =
            { position = startPoint
            , index = i
            }

        endVertex =
            { position = endPoint
            , index = j
            }

        ( newEdge, state1 ) =
            buildEdge (subdivisionLevels subdivisionFunction startPoint endPoint)
                startVertex
                endVertex
                state0
    in
    ( newEdge
    , { vertexCount = state1.vertexCount
      , vertexStack = state1.vertexStack
      , edgeDict = Dict.insert ( i, j ) newEdge state1.edgeDict
      , accumulatedFaces = state1.accumulatedFaces
      }
    )


getOrCreateEdge :
    TriangularMesh (Point2d units coordinates)
    -> SubdivisionFunction units coordinates
    -> Int
    -> Int
    -> State units coordinates
    -> ( Maybe (Edge units coordinates), State units coordinates )
getOrCreateEdge triangularMesh subdivisionFunction i j state0 =
    case Dict.get ( j, i ) state0.edgeDict of
        Just oppositeEdge ->
            -- The opposite edge exists, use it
            -- Note: don't bother adding the newly created edge to the dict,
            -- since each edge should only be reused once
            ( Just (reverseEdge oppositeEdge), state0 )

        Nothing ->
            -- No opposite edge exists, so create a new edge and store it
            case
                ( TriangularMesh.vertex i triangularMesh
                , TriangularMesh.vertex j triangularMesh
                )
            of
                ( Just startPoint, Just endPoint ) ->
                    Tuple.mapFirst Just <|
                        createEdge triangularMesh subdivisionFunction i j startPoint endPoint state0

                _ ->
                    -- Shouldn't happen
                    ( Nothing, state0 )


initFaces :
    TriangularMesh (Point2d units coordinates)
    -> SubdivisionFunction units coordinates
    -> List ( Int, Int, Int )
    -> List (Face units coordinates)
    -> State units coordinates
    -> ( List (Face units coordinates), State units coordinates )
initFaces triangularMesh subdivisionFunction faceIndices accumulated state0 =
    case faceIndices of
        ( i, j, k ) :: rest ->
            let
                ( maybeEdge1, state1 ) =
                    getOrCreateEdge triangularMesh subdivisionFunction i j state0

                ( maybeEdge2, state2 ) =
                    getOrCreateEdge triangularMesh subdivisionFunction j k state1

                ( maybeEdge3, state3 ) =
                    getOrCreateEdge triangularMesh subdivisionFunction k i state2

                maybeFace =
                    Maybe.map3 (\edge1 edge2 edge3 -> ( edge1, edge2, edge3 ))
                        maybeEdge1
                        maybeEdge2
                        maybeEdge3
            in
            case maybeFace of
                Just face ->
                    initFaces triangularMesh subdivisionFunction rest (face :: accumulated) state3

                Nothing ->
                    initFaces triangularMesh subdivisionFunction rest accumulated state0

        [] ->
            ( accumulated, state0 )


addFace :
    Vertex units coordinates
    -> Vertex units coordinates
    -> Vertex units coordinates
    -> State units coordinates
    -> State units coordinates
addFace v1 v2 v3 currentState =
    { vertexStack = currentState.vertexStack
    , vertexCount = currentState.vertexCount
    , edgeDict = currentState.edgeDict
    , accumulatedFaces =
        ( v1.index, v2.index, v3.index ) :: currentState.accumulatedFaces
    }


edgeStart : Edge units coordinates -> Vertex units coordinates
edgeStart edge =
    case edge of
        Short { start } ->
            start

        Long { start } ->
            start


edgeEnd : Edge units coordinates -> Vertex units coordinates
edgeEnd edge =
    case edge of
        Short { end } ->
            end

        Long { end } ->
            end


edgeMid : LongEdge units coordinates -> Vertex units coordinates
edgeMid { firstEdge } =
    edgeEnd firstEdge


processSingleLongEdge :
    SubdivisionFunction units coordinates
    -> ShortEdge units coordinates
    -> ShortEdge units coordinates
    -> LongEdge units coordinates
    -> State units coordinates
    -> State units coordinates
processSingleLongEdge subdivisionFunction e1 e2 e3 state =
    let
        midVertex =
            edgeMid e3

        f1 =
            ( Short e1
            , Short { start = e1.end, end = midVertex }
            , e3.secondEdge
            )

        f2 =
            ( Short e2
            , e3.firstEdge
            , Short { start = midVertex, end = e1.end }
            )
    in
    state
        |> processFace subdivisionFunction f1
        |> processFace subdivisionFunction f2


processTrapezoid :
    SubdivisionFunction units coordinates
    -> ShortEdge units coordinates
    -> Edge units coordinates
    -> ShortEdge units coordinates
    -> Edge units coordinates
    -> State units coordinates
    -> State units coordinates
processTrapezoid subdivisionFunction bottom right top left state0 =
    case ( left, right ) of
        ( Short leftEdge, Short rightEdge ) ->
            let
                desiredSubdivisions1 =
                    subdivisionFunction bottom.start.position top.start.position

                desiredSubdivisions2 =
                    subdivisionFunction bottom.end.position top.end.position
            in
            if desiredSubdivisions1 > 1 && desiredSubdivisions2 > 1 then
                -- Both diagonals are too long, create a middle vertex and
                -- create four faces using it
                let
                    midpoint =
                        Point2d.midpoint bottom.start.position top.start.position

                    ( midVertex, state1 ) =
                        addVertex midpoint state0
                in
                state1
                    |> addFace bottom.start bottom.end midVertex
                    |> addFace bottom.end top.start midVertex
                    |> addFace top.start top.end midVertex
                    |> addFace top.end bottom.start midVertex

            else if desiredSubdivisions1 <= desiredSubdivisions2 then
                -- Create diagonal from bottom start to top start
                state0
                    |> addFace bottom.start bottom.end top.start
                    |> addFace top.start top.end bottom.start

            else
                -- Create diagonal from bottom end to top end
                state0
                    |> addFace bottom.start bottom.end top.end
                    |> addFace bottom.end top.start top.end

        ( Short leftEdge, Long rightEdge ) ->
            let
                midpoint =
                    edgeMid rightEdge

                upperSubdivisionLevels =
                    subdivisionLevels subdivisionFunction top.end.position midpoint.position

                ( upperDiagonal, state1 ) =
                    buildEdge upperSubdivisionLevels top.end midpoint state0

                lowerSubdivisionLevels =
                    subdivisionLevels subdivisionFunction bottom.start.position midpoint.position

                ( lowerDiagonal, state2 ) =
                    buildEdge lowerSubdivisionLevels bottom.start midpoint state1
            in
            state2
                |> processFace subdivisionFunction
                    ( Short bottom, rightEdge.firstEdge, reverseEdge lowerDiagonal )
                |> processFace subdivisionFunction
                    ( lowerDiagonal, reverseEdge upperDiagonal, left )
                |> processFace subdivisionFunction
                    ( rightEdge.secondEdge, Short top, upperDiagonal )

        ( Long leftEdge, Short rightEdge ) ->
            let
                midpoint =
                    edgeMid leftEdge

                upperSubdivisionLevels =
                    subdivisionLevels subdivisionFunction top.start.position midpoint.position

                ( upperDiagonal, state1 ) =
                    buildEdge upperSubdivisionLevels top.start midpoint state0

                lowerSubdivisionLevels =
                    subdivisionLevels subdivisionFunction bottom.end.position midpoint.position

                ( lowerDiagonal, state2 ) =
                    buildEdge lowerSubdivisionLevels bottom.end midpoint state1
            in
            state2
                |> processFace subdivisionFunction
                    ( Short bottom, lowerDiagonal, leftEdge.secondEdge )
                |> processFace subdivisionFunction
                    ( reverseEdge lowerDiagonal, right, upperDiagonal )
                |> processFace subdivisionFunction
                    ( leftEdge.firstEdge, reverseEdge upperDiagonal, Short top )

        ( Long leftEdge, Long rightEdge ) ->
            let
                leftMid =
                    edgeMid leftEdge

                rightMid =
                    edgeMid rightEdge

                lowerTopEdge =
                    { start = rightMid, end = leftMid }

                upperBottomEdge =
                    { start = leftMid, end = rightMid }
            in
            state0
                |> processTrapezoid subdivisionFunction
                    bottom
                    rightEdge.firstEdge
                    lowerTopEdge
                    leftEdge.secondEdge
                |> processTrapezoid subdivisionFunction
                    upperBottomEdge
                    rightEdge.secondEdge
                    top
                    leftEdge.firstEdge


processSingleShortEdge :
    SubdivisionFunction units coordinates
    -> ShortEdge units coordinates
    -> LongEdge units coordinates
    -> LongEdge units coordinates
    -> State units coordinates
    -> State units coordinates
processSingleShortEdge subdivisionFunction e1 e2 e3 state =
    let
        mid2 =
            edgeMid e2

        mid3 =
            edgeMid e3

        triangleBase =
            { start = mid3, end = mid2 }

        trapezoidTop =
            { start = mid2, end = mid3 }
    in
    state
        |> processFace subdivisionFunction ( e2.secondEdge, e3.firstEdge, Short triangleBase )
        |> processTrapezoid subdivisionFunction e1 e2.firstEdge trapezoidTop e3.secondEdge


processThreeLongEdges :
    SubdivisionFunction units coordinates
    -> LongEdge units coordinates
    -> LongEdge units coordinates
    -> LongEdge units coordinates
    -> State units coordinates
    -> State units coordinates
processThreeLongEdges subdivisionFunction e1 e2 e3 state0 =
    let
        v1 =
            edgeMid e1

        v2 =
            edgeMid e2

        v3 =
            edgeMid e3

        ( p1, state1 ) =
            buildEdge (e1.level - 1) v2 v3 state0

        ( p2, state2 ) =
            buildEdge (e2.level - 1) v3 v1 state1

        ( p3, state3 ) =
            buildEdge (e3.level - 1) v1 v2 state2

        f1 =
            ( e1.firstEdge, reverseEdge p2, e3.secondEdge )

        f2 =
            ( e2.firstEdge, reverseEdge p3, e1.secondEdge )

        f3 =
            ( e3.firstEdge, reverseEdge p1, e2.secondEdge )

        f4 =
            ( p1, p2, p3 )
    in
    state3
        |> processFace subdivisionFunction f1
        |> processFace subdivisionFunction f2
        |> processFace subdivisionFunction f3
        |> processFace subdivisionFunction f4


processFace :
    SubdivisionFunction units coordinates
    -> Face units coordinates
    -> State units coordinates
    -> State units coordinates
processFace subdivisionFunction face state =
    case face of
        -- All short edges
        ( Short e1, Short e2, Short e3 ) ->
            addFace e1.start e2.start e3.start state

        -- One long edge
        ( Short e1, Short e2, Long e3 ) ->
            processSingleLongEdge subdivisionFunction e1 e2 e3 state

        ( Short e1, Long e2, Short e3 ) ->
            processSingleLongEdge subdivisionFunction e3 e1 e2 state

        ( Long e1, Short e2, Short e3 ) ->
            processSingleLongEdge subdivisionFunction e2 e3 e1 state

        -- One short edge
        ( Long e1, Long e2, Short e3 ) ->
            processSingleShortEdge subdivisionFunction e3 e1 e2 state

        ( Long e1, Short e2, Long e3 ) ->
            processSingleShortEdge subdivisionFunction e2 e3 e1 state

        ( Short e1, Long e2, Long e3 ) ->
            processSingleShortEdge subdivisionFunction e1 e2 e3 state

        -- All long edges
        ( Long e1, Long e2, Long e3 ) ->
            processThreeLongEdges subdivisionFunction e1 e2 e3 state


refine :
    (Point2d units coordinates -> Point2d units coordinates -> Int)
    -> TriangularMesh (Point2d units coordinates)
    -> TriangularMesh (Point2d units coordinates)
refine subdivisionFunction triangularMesh =
    let
        state0 =
            initState triangularMesh

        ( initialFaces, state1 ) =
            initFaces triangularMesh
                subdivisionFunction
                (TriangularMesh.faceIndices triangularMesh)
                []
                state0

        state2 =
            List.foldl (processFace subdivisionFunction) state1 initialFaces

        accumulatedVertices =
            Array.fromList (List.reverse state2.vertexStack)
    in
    TriangularMesh.indexed accumulatedVertices state2.accumulatedFaces
