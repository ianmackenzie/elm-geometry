module Polygon2d.Refinement exposing (refine)

import Array
import Dict exposing (Dict)
import Point2d exposing (Point2d)
import TriangularMesh exposing (TriangularMesh)


type alias VertexRecord vertex units coordinates =
    { vertex : vertex
    , position : Point2d units coordinates
    , index : Int
    }


type alias ShortEdge vertex units coordinates =
    { start : VertexRecord vertex units coordinates
    , end : VertexRecord vertex units coordinates
    }


type alias LongEdge vertex units coordinates =
    { start : VertexRecord vertex units coordinates
    , firstEdge : Edge vertex units coordinates
    , secondEdge : Edge vertex units coordinates
    , end : VertexRecord vertex units coordinates
    , level : Int
    }


type Edge vertex units coordinates
    = Short (ShortEdge vertex units coordinates)
    | Long (LongEdge vertex units coordinates)


type alias State vertex units coordinates =
    { vertexStack : List vertex
    , vertexCount : Int
    , edgeDict : Dict ( Int, Int ) (Edge vertex units coordinates)
    , accumulatedFaces : List ( Int, Int, Int )
    }


type alias Face vertex units coordinates =
    ( Edge vertex units coordinates, Edge vertex units coordinates, Edge vertex units coordinates )


type alias SubdivisionFunction units coordinates =
    Point2d units coordinates -> Point2d units coordinates -> Int


type alias Config vertex units coordinates =
    { vertexPosition : vertex -> Point2d units coordinates
    , midpoint : vertex -> vertex -> vertex
    , subdivisionFunction : SubdivisionFunction units coordinates
    }


initState : TriangularMesh vertex -> State vertex units coordinates
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
    Config vertex units coordinates
    -> vertex
    -> State vertex units coordinates
    -> ( VertexRecord vertex units coordinates, State vertex units coordinates )
addVertex config vertex state =
    let
        currentCount =
            state.vertexCount
    in
    ( { vertex = vertex
      , position = config.vertexPosition vertex
      , index = currentCount
      }
    , { vertexStack = vertex :: state.vertexStack
      , vertexCount = currentCount + 1
      , edgeDict = state.edgeDict
      , accumulatedFaces = state.accumulatedFaces
      }
    )


reverseEdge : Edge vertex units coordinates -> Edge vertex units coordinates
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
    Config vertex units coordinates
    -> Int
    -> VertexRecord vertex units coordinates
    -> VertexRecord vertex units coordinates
    -> State vertex units coordinates
    -> ( Edge vertex units coordinates, State vertex units coordinates )
buildEdge config level startVertexRecord endVertexRecord state0 =
    if level <= 0 then
        ( Short { start = startVertexRecord, end = endVertexRecord }, state0 )

    else
        let
            newVertex =
                config.midpoint startVertexRecord.vertex endVertexRecord.vertex

            ( midVertexRecord, state1 ) =
                addVertex config newVertex state0

            ( firstEdge, state2 ) =
                buildEdge config (level - 1) startVertexRecord midVertexRecord state1

            ( secondEdge, state3 ) =
                buildEdge config (level - 1) midVertexRecord endVertexRecord state2
        in
        ( Long
            { start = startVertexRecord
            , firstEdge = firstEdge
            , secondEdge = secondEdge
            , end = endVertexRecord
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
    Config vertex units coordinates
    -> Int
    -> Int
    -> vertex
    -> vertex
    -> State vertex units coordinates
    -> ( Edge vertex units coordinates, State vertex units coordinates )
createEdge config i j startVertex endVertex state0 =
    let
        startPoint =
            config.vertexPosition startVertex

        endPoint =
            config.vertexPosition endVertex

        startVertexRecord =
            { vertex = startVertex
            , position = startPoint
            , index = i
            }

        endVertexRecord =
            { vertex = endVertex
            , position = endPoint
            , index = j
            }

        ( newEdge, state1 ) =
            buildEdge config
                (subdivisionLevels config.subdivisionFunction startPoint endPoint)
                startVertexRecord
                endVertexRecord
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
    Config vertex units coordinates
    -> TriangularMesh vertex
    -> Int
    -> Int
    -> State vertex units coordinates
    -> ( Maybe (Edge vertex units coordinates), State vertex units coordinates )
getOrCreateEdge config triangularMesh i j state0 =
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
                ( Just startVertex, Just endVertex ) ->
                    Tuple.mapFirst Just <|
                        createEdge config i j startVertex endVertex state0

                _ ->
                    -- Shouldn't happen
                    ( Nothing, state0 )


initFaces :
    Config vertex units coordinates
    -> TriangularMesh vertex
    -> List ( Int, Int, Int )
    -> List (Face vertex units coordinates)
    -> State vertex units coordinates
    -> ( List (Face vertex units coordinates), State vertex units coordinates )
initFaces config triangularMesh faceIndices accumulated state0 =
    case faceIndices of
        ( i, j, k ) :: rest ->
            let
                ( maybeEdge1, state1 ) =
                    getOrCreateEdge config triangularMesh i j state0

                ( maybeEdge2, state2 ) =
                    getOrCreateEdge config triangularMesh j k state1

                ( maybeEdge3, state3 ) =
                    getOrCreateEdge config triangularMesh k i state2

                maybeFace =
                    Maybe.map3 (\edge1 edge2 edge3 -> ( edge1, edge2, edge3 ))
                        maybeEdge1
                        maybeEdge2
                        maybeEdge3
            in
            case maybeFace of
                Just face ->
                    initFaces config triangularMesh rest (face :: accumulated) state3

                Nothing ->
                    initFaces config triangularMesh rest accumulated state0

        [] ->
            ( accumulated, state0 )


addFace :
    VertexRecord vertex units coordinates
    -> VertexRecord vertex units coordinates
    -> VertexRecord vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
addFace v1 v2 v3 currentState =
    { vertexStack = currentState.vertexStack
    , vertexCount = currentState.vertexCount
    , edgeDict = currentState.edgeDict
    , accumulatedFaces =
        ( v1.index, v2.index, v3.index ) :: currentState.accumulatedFaces
    }


edgeStart : Edge vertex units coordinates -> VertexRecord vertex units coordinates
edgeStart edge =
    case edge of
        Short { start } ->
            start

        Long { start } ->
            start


edgeEnd : Edge vertex units coordinates -> VertexRecord vertex units coordinates
edgeEnd edge =
    case edge of
        Short { end } ->
            end

        Long { end } ->
            end


edgeMid : LongEdge vertex units coordinates -> VertexRecord vertex units coordinates
edgeMid { firstEdge } =
    edgeEnd firstEdge


processSingleLongEdge :
    Config vertex units coordinates
    -> ShortEdge vertex units coordinates
    -> ShortEdge vertex units coordinates
    -> LongEdge vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
processSingleLongEdge config e1 e2 e3 state =
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
        |> processFace config f1
        |> processFace config f2


processTrapezoid :
    Config vertex units coordinates
    -> ShortEdge vertex units coordinates
    -> Edge vertex units coordinates
    -> ShortEdge vertex units coordinates
    -> Edge vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
processTrapezoid config bottom right top left state0 =
    let
        subdivisionFunction =
            config.subdivisionFunction
    in
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
                    newVertex =
                        config.midpoint bottom.start.vertex top.start.vertex

                    ( midVertex, state1 ) =
                        addVertex config newVertex state0
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
                    buildEdge config upperSubdivisionLevels top.end midpoint state0

                lowerSubdivisionLevels =
                    subdivisionLevels subdivisionFunction bottom.start.position midpoint.position

                ( lowerDiagonal, state2 ) =
                    buildEdge config lowerSubdivisionLevels bottom.start midpoint state1
            in
            state2
                |> processFace config ( Short bottom, rightEdge.firstEdge, reverseEdge lowerDiagonal )
                |> processFace config ( lowerDiagonal, reverseEdge upperDiagonal, left )
                |> processFace config ( rightEdge.secondEdge, Short top, upperDiagonal )

        ( Long leftEdge, Short rightEdge ) ->
            let
                midpoint =
                    edgeMid leftEdge

                upperSubdivisionLevels =
                    subdivisionLevels subdivisionFunction top.start.position midpoint.position

                ( upperDiagonal, state1 ) =
                    buildEdge config upperSubdivisionLevels top.start midpoint state0

                lowerSubdivisionLevels =
                    subdivisionLevels subdivisionFunction bottom.end.position midpoint.position

                ( lowerDiagonal, state2 ) =
                    buildEdge config lowerSubdivisionLevels bottom.end midpoint state1
            in
            state2
                |> processFace config ( Short bottom, lowerDiagonal, leftEdge.secondEdge )
                |> processFace config ( reverseEdge lowerDiagonal, right, upperDiagonal )
                |> processFace config ( leftEdge.firstEdge, reverseEdge upperDiagonal, Short top )

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
                |> processTrapezoid config
                    bottom
                    rightEdge.firstEdge
                    lowerTopEdge
                    leftEdge.secondEdge
                |> processTrapezoid config
                    upperBottomEdge
                    rightEdge.secondEdge
                    top
                    leftEdge.firstEdge


processSingleShortEdge :
    Config vertex units coordinates
    -> ShortEdge vertex units coordinates
    -> LongEdge vertex units coordinates
    -> LongEdge vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
processSingleShortEdge config e1 e2 e3 state =
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
        |> processFace config ( e2.secondEdge, e3.firstEdge, Short triangleBase )
        |> processTrapezoid config e1 e2.firstEdge trapezoidTop e3.secondEdge


processThreeLongEdges :
    Config vertex units coordinates
    -> LongEdge vertex units coordinates
    -> LongEdge vertex units coordinates
    -> LongEdge vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
processThreeLongEdges config e1 e2 e3 state0 =
    let
        v1 =
            edgeMid e1

        v2 =
            edgeMid e2

        v3 =
            edgeMid e3

        ( p1, state1 ) =
            buildEdge config (e1.level - 1) v2 v3 state0

        ( p2, state2 ) =
            buildEdge config (e2.level - 1) v3 v1 state1

        ( p3, state3 ) =
            buildEdge config (e3.level - 1) v1 v2 state2

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
        |> processFace config f1
        |> processFace config f2
        |> processFace config f3
        |> processFace config f4


processFace :
    Config vertex units coordinates
    -> Face vertex units coordinates
    -> State vertex units coordinates
    -> State vertex units coordinates
processFace config face state =
    case face of
        -- All short edges
        ( Short e1, Short e2, Short e3 ) ->
            addFace e1.start e2.start e3.start state

        -- One long edge
        ( Short e1, Short e2, Long e3 ) ->
            processSingleLongEdge config e1 e2 e3 state

        ( Short e1, Long e2, Short e3 ) ->
            processSingleLongEdge config e3 e1 e2 state

        ( Long e1, Short e2, Short e3 ) ->
            processSingleLongEdge config e2 e3 e1 state

        -- One short edge
        ( Long e1, Long e2, Short e3 ) ->
            processSingleShortEdge config e3 e1 e2 state

        ( Long e1, Short e2, Long e3 ) ->
            processSingleShortEdge config e2 e3 e1 state

        ( Short e1, Long e2, Long e3 ) ->
            processSingleShortEdge config e1 e2 e3 state

        -- All long edges
        ( Long e1, Long e2, Long e3 ) ->
            processThreeLongEdges config e1 e2 e3 state


refine :
    Config vertex units coordinates
    -> TriangularMesh vertex
    -> TriangularMesh vertex
refine config triangularMesh =
    let
        state0 =
            initState triangularMesh

        ( initialFaces, state1 ) =
            initFaces config
                triangularMesh
                (TriangularMesh.faceIndices triangularMesh)
                []
                state0

        state2 =
            List.foldl (processFace config) state1 initialFaces

        accumulatedVertices =
            Array.fromList (List.reverse state2.vertexStack)
    in
    TriangularMesh.indexed accumulatedVertices state2.accumulatedFaces
