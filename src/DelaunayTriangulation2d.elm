module DelaunayTriangulation2d
    exposing
        ( DelaunayTriangulation2d
        , Face
        , circumcircles
        , empty
        , faces
        , fromPoints
        , fromVerticesBy
        , insertPoint
        , insertVertexBy
        , toMesh
        , triangles
        , vertices
        , voronoiRegions
        )

import Array exposing (Array)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (DelaunayFace(..), DelaunayVertex)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)


type alias DelaunayTriangulation2d vertex =
    Types.DelaunayTriangulation2d vertex


type alias Face vertex =
    { vertices : ( vertex, vertex, vertex )
    , triangle : Triangle2d
    , circumcircle : Circle2d
    }


empty : DelaunayTriangulation2d vertex
empty =
    Types.EmptyDelaunayTriangulation2d


fromPoints : Array Point2d -> DelaunayTriangulation2d Point2d
fromPoints points =
    fromVerticesBy identity points


insertVertex : (vertex -> Point2d) -> vertex -> ( Int, Dict ( Float, Float ) (DelaunayVertex vertex) ) -> ( Int, Dict ( Float, Float ) (DelaunayVertex vertex) )
insertVertex getPosition vertex ( index, dict ) =
    let
        position =
            getPosition vertex

        delaunayVertex =
            { vertex = vertex
            , index = index
            , position = position
            }

        updatedDict =
            dict |> Dict.insert (Point2d.coordinates position) delaunayVertex
    in
    ( index + 1, updatedDict )


createInitialFaces : DelaunayVertex vertex -> List (DelaunayFace vertex)
createInitialFaces firstVertex =
    let
        firstPoint =
            firstVertex.position

        topIndex =
            -1

        leftIndex =
            -2

        rightIndex =
            -3
    in
    [ OneVertexFace firstVertex topIndex leftIndex Direction2d.positiveY
    , OneVertexFace firstVertex leftIndex rightIndex Direction2d.negativeX
    , OneVertexFace firstVertex rightIndex topIndex Direction2d.negativeY
    ]


fromVerticesBy : (vertex -> Point2d) -> Array vertex -> DelaunayTriangulation2d vertex
fromVerticesBy getPosition givenVertices =
    let
        delaunayVertices =
            givenVertices
                |> Array.foldl (insertVertex getPosition) ( 0, Dict.empty )
                |> Tuple.second
                |> Dict.values
    in
    case delaunayVertices of
        firstDelaunayVertex :: remainingDelaunayVertices ->
            let
                initialFaces =
                    createInitialFaces firstDelaunayVertex

                faces_ =
                    List.foldl addVertex initialFaces remainingDelaunayVertices
            in
            Types.DelaunayTriangulation2d
                { vertices = givenVertices
                , delaunayVertices = delaunayVertices
                , faces = faces_
                }

        [] ->
            Types.EmptyDelaunayTriangulation2d


insertPoint : Point2d -> DelaunayTriangulation2d Point2d -> DelaunayTriangulation2d Point2d
insertPoint point delaunayTriangulation =
    insertVertexBy identity point delaunayTriangulation


isNewPoint : Point2d -> List (DelaunayVertex vertex) -> Bool
isNewPoint point delaunayVertices =
    case delaunayVertices of
        [] ->
            True

        firstDelaunayVertex :: remainingDelaunayVertices ->
            if point == firstDelaunayVertex.position then
                False
            else
                isNewPoint point remainingDelaunayVertices


insertVertexBy : (vertex -> Point2d) -> vertex -> DelaunayTriangulation2d vertex -> DelaunayTriangulation2d vertex
insertVertexBy getPosition vertex delaunayTriangulation =
    let
        position =
            getPosition vertex
    in
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            let
                initialDelaunayVertex =
                    { vertex = vertex
                    , position = position
                    , index = 0
                    }
            in
            Types.DelaunayTriangulation2d
                { vertices = Array.repeat 1 vertex
                , faces = createInitialFaces initialDelaunayVertex
                , delaunayVertices = [ initialDelaunayVertex ]
                }

        Types.DelaunayTriangulation2d current ->
            if isNewPoint position current.delaunayVertices then
                let
                    newDelaunayVertex =
                        { vertex = vertex
                        , position = position
                        , index = Array.length current.vertices
                        }
                in
                Types.DelaunayTriangulation2d
                    { vertices = current.vertices |> Array.push vertex
                    , delaunayVertices =
                        newDelaunayVertex :: current.delaunayVertices
                    , faces = addVertex newDelaunayVertex current.faces
                    }
            else
                delaunayTriangulation


collectHelp : (DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> a) -> List (DelaunayFace vertex) -> List a -> List a
collectHelp function facesToProcess accumulated =
    case facesToProcess of
        firstFace :: remainingFaces ->
            case firstFace of
                ThreeVertexFace firstVertex secondVertex thirdVertex circumcircle ->
                    let
                        newValue =
                            function
                                firstVertex
                                secondVertex
                                thirdVertex
                                circumcircle

                        updated =
                            newValue :: accumulated
                    in
                    collectHelp function remainingFaces updated

                _ ->
                    collectHelp function remainingFaces accumulated

        [] ->
            accumulated


collectFaces : (DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> a) -> DelaunayTriangulation2d vertex -> List a
collectFaces function delaunayTriangulation =
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            []

        Types.DelaunayTriangulation2d triangulation ->
            collectHelp function triangulation.faces []


getFaceIndices : DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> ( Int, Int, Int )
getFaceIndices firstVertex secondVertex thirdVertex circumcircle =
    ( firstVertex.index, secondVertex.index, thirdVertex.index )


toMesh : DelaunayTriangulation2d vertex -> TriangularMesh vertex
toMesh delaunayTriangulation =
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            TriangularMesh.empty

        Types.DelaunayTriangulation2d triangulation ->
            let
                faceIndices =
                    collectFaces getFaceIndices delaunayTriangulation
            in
            TriangularMesh.indexed triangulation.vertices faceIndices


getTriangle : DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> Triangle2d
getTriangle firstVertex secondVertex thirdVertex circumcircle =
    Triangle2d.fromVertices
        ( firstVertex.position
        , secondVertex.position
        , thirdVertex.position
        )


triangles : DelaunayTriangulation2d vertex -> List Triangle2d
triangles delaunayTriangulation =
    collectFaces getTriangle delaunayTriangulation


getCircumcircle : DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> Circle2d
getCircumcircle firstVertex secondVertex thirdVertex circumcircle =
    circumcircle


circumcircles : DelaunayTriangulation2d vertex -> List Circle2d
circumcircles delaunayTriangulation =
    collectFaces getCircumcircle delaunayTriangulation


getFace : DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> Face vertex
getFace firstVertex secondVertex thirdVertex circumcircle =
    { vertices = ( firstVertex.vertex, secondVertex.vertex, thirdVertex.vertex )
    , triangle =
        Triangle2d.fromVertices
            ( firstVertex.position
            , secondVertex.position
            , thirdVertex.position
            )
    , circumcircle = circumcircle
    }


faces : DelaunayTriangulation2d vertex -> List (Face vertex)
faces delaunayTriangulation =
    collectFaces getFace delaunayTriangulation


vertices : DelaunayTriangulation2d vertex -> Array vertex
vertices delaunayTriangulation =
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            Array.empty

        Types.DelaunayTriangulation2d triangulation ->
            triangulation.vertices


type Edge vertex
    = InnerEdge (DelaunayVertex vertex) (DelaunayVertex vertex)
    | InnerToOuterEdge (DelaunayVertex vertex) Int
    | OuterToInnerEdge Int (DelaunayVertex vertex)
    | OuterEdge Direction2d Int Int


edgeKey : Int -> Int -> Float
edgeKey i j =
    let
        x =
            toFloat (i + 3)

        y =
            toFloat (j + 3)
    in
    if x >= y then
        x * x + y
    else
        y * y + x


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


addEdge : Edge vertex -> Maybe (Edge vertex) -> Maybe (Edge vertex)
addEdge newEdge maybeEdge =
    case maybeEdge of
        Just edge ->
            Nothing

        Nothing ->
            Just newEdge


processFaces : List (DelaunayFace vertex) -> DelaunayVertex vertex -> List (DelaunayFace vertex) -> Dict Float (Edge vertex) -> ( List (DelaunayFace vertex), Dict Float (Edge vertex) )
processFaces facesToProcess newVertex retainedFaces edgesByKey =
    case facesToProcess of
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
                                edgeKey firstIndex secondIndex

                            edge1 =
                                InnerEdge firstVertex secondVertex

                            key2 =
                                edgeKey secondIndex thirdIndex

                            edge2 =
                                InnerEdge secondVertex thirdVertex

                            key3 =
                                edgeKey thirdIndex firstIndex

                            edge3 =
                                InnerEdge thirdVertex firstVertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
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
                                edgeKey firstIndex secondIndex

                            edge1 =
                                InnerEdge firstVertex secondVertex

                            key2 =
                                edgeKey secondIndex outerIndex

                            edge2 =
                                InnerToOuterEdge secondVertex outerIndex

                            key3 =
                                edgeKey outerIndex firstIndex

                            edge3 =
                                OuterToInnerEdge outerIndex firstVertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
                            newVertex
                            (firstFace :: retainedFaces)
                            edgesByKey

                OneVertexFace delaunayVertex firstOuterIndex secondOuterIndex edgeDirection ->
                    let
                        insideInfiniteCircle =
                            signedDistance
                                newVertex.position
                                delaunayVertex.position
                                edgeDirection
                                < 0
                    in
                    if insideInfiniteCircle then
                        let
                            vertexIndex =
                                delaunayVertex.index

                            key1 =
                                edgeKey vertexIndex firstOuterIndex

                            edge1 =
                                InnerToOuterEdge delaunayVertex firstOuterIndex

                            key2 =
                                edgeKey firstOuterIndex secondOuterIndex

                            edge2 =
                                OuterEdge edgeDirection
                                    firstOuterIndex
                                    secondOuterIndex

                            key3 =
                                edgeKey secondOuterIndex vertexIndex

                            edge3 =
                                OuterToInnerEdge secondOuterIndex delaunayVertex

                            updatedEdges =
                                edgesByKey
                                    |> Dict.update key1 (addEdge edge1)
                                    |> Dict.update key2 (addEdge edge2)
                                    |> Dict.update key3 (addEdge edge3)
                        in
                        processFaces remainingFaces
                            newVertex
                            retainedFaces
                            updatedEdges
                    else
                        processFaces remainingFaces
                            newVertex
                            (firstFace :: retainedFaces)
                            edgesByKey

        [] ->
            ( retainedFaces, edgesByKey )


addNewFace : DelaunayVertex vertex -> Float -> Edge vertex -> List (DelaunayFace vertex) -> List (DelaunayFace vertex)
addNewFace newVertex ignoredEdgeKey edge currentFaces =
    case edge of
        InnerEdge firstDelaunayVertex secondDelaunayVertex ->
            let
                maybeCircumcircle =
                    Circle2d.throughPoints
                        newVertex.position
                        firstDelaunayVertex.position
                        secondDelaunayVertex.position
            in
            case maybeCircumcircle of
                Just circumcircle ->
                    let
                        newFace =
                            ThreeVertexFace
                                newVertex
                                firstDelaunayVertex
                                secondDelaunayVertex
                                circumcircle
                    in
                    newFace :: currentFaces

                Nothing ->
                    currentFaces

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
                    newFace :: currentFaces

                Nothing ->
                    currentFaces

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
                    newFace :: currentFaces

                Nothing ->
                    currentFaces

        OuterEdge edgeDirection firstOuterIndex secondOuterIndex ->
            let
                newFace =
                    OneVertexFace newVertex
                        firstOuterIndex
                        secondOuterIndex
                        edgeDirection
            in
            newFace :: currentFaces


addVertex : DelaunayVertex vertex -> List (DelaunayFace vertex) -> List (DelaunayFace vertex)
addVertex vertex faces_ =
    let
        ( retainedFaces, starEdges ) =
            processFaces faces_ vertex [] Dict.empty
    in
    starEdges |> Dict.foldl (addNewFace vertex) retainedFaces



---------- VORONOI REGIONS ----------


type alias VoronoiAccumulator =
    { points : List Point2d
    , startDirection : Maybe Direction2d
    , endDirection : Maybe Direction2d
    }


addInterior : Point2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
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


addStartDirection : Direction2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
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


addEndDirection : Direction2d -> Maybe VoronoiAccumulator -> Maybe VoronoiAccumulator
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


accumulateRegions : DelaunayFace vertex -> Dict Int VoronoiAccumulator -> Dict Int VoronoiAccumulator
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


createRegion : Dict Int VoronoiAccumulator -> DelaunayVertex vertex -> VoronoiRegions vertex -> VoronoiRegions vertex
createRegion accumulatorsByIndex delaunayVertex regions =
    case Dict.get delaunayVertex.index accumulatorsByIndex of
        Just { points, startDirection, endDirection } ->
            case ( startDirection, endDirection ) of
                ( Nothing, Nothing ) ->
                    let
                        sortedPoints =
                            points
                                |> List.sortBy
                                    (pseudoAngle delaunayVertex.position)

                        finiteRegion =
                            { vertex = delaunayVertex.vertex
                            , region = Polygon2d.singleLoop sortedPoints
                            }
                    in
                    { regions | finite = finiteRegion :: regions.finite }

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

                                infiniteRegion =
                                    { vertex = delaunayVertex.vertex
                                    , leftAxis = leftAxis
                                    , rightAxis = rightAxis
                                    , finitePortion = polyline
                                    }
                            in
                            { regions
                                | infinite = infiniteRegion :: regions.infinite
                            }

                        [] ->
                            regions

                _ ->
                    regions

        Nothing ->
            regions


type alias VoronoiRegions vertex =
    { finite : List { vertex : vertex, region : Polygon2d }
    , infinite : List { vertex : vertex, leftAxis : Axis2d, rightAxis : Axis2d, finitePortion : Polyline2d }
    }


emptyRegions : VoronoiRegions vertex
emptyRegions =
    { finite = []
    , infinite = []
    }


voronoiRegions : DelaunayTriangulation2d vertex -> VoronoiRegions vertex
voronoiRegions delaunayTriangulation =
    case delaunayTriangulation of
        Types.EmptyDelaunayTriangulation2d ->
            emptyRegions

        Types.DelaunayTriangulation2d triangulation ->
            let
                accumulators =
                    List.foldl accumulateRegions Dict.empty triangulation.faces
            in
            List.foldl (createRegion accumulators)
                emptyRegions
                triangulation.delaunayVertices
