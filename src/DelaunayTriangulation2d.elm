--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module DelaunayTriangulation2d exposing
    ( DelaunayTriangulation2d, Error(..), Face
    , empty
    , fromPoints, fromVerticesBy
    , insertPoint, insertVertexBy
    , vertices, triangles, circumcircles, faces, toMesh
    )

{-| This module provides functionality for working with [Delaunay
triangulations](https://en.wikipedia.org/wiki/Delaunay_triangulation).

![Delaunay triangulation](https://ianmackenzie.github.io/elm-geometry/1.2.0/DelaunayTriangulation2d/DelaunayTriangulation.png)

You can:

  - Build a Delaunay triangulation from a set of points or arbitrary vertices
  - Add a new vertex to an existing Delaunay triangulation
  - Extract the resulting triangulation as a list of triangles or a
    [`TriangularMesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/TriangularMesh#TriangularMesh)

The current implementation is somewhat inefficient, but there are plans to speed
it up in the future (without requiring any changes to the API).

@docs DelaunayTriangulation2d, Error, Face


# Construction

Constructing a Delaunay triangulation from points/vertices is currently an
O(n^2) operation but should be O(n log n) in the future.

@docs empty

@docs fromPoints, fromVerticesBy


# Modification

Inserting a point into a Delaunay triangulation is currently an O(n) operation
but should be O(log n) in the future.

@docs insertPoint, insertVertexBy


# Properties

@docs vertices, triangles, circumcircles, faces, toMesh

-}

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


{-| A 2D Delaunay triangulation of a set of vertices.
-}
type alias DelaunayTriangulation2d vertex =
    Types.DelaunayTriangulation2d vertex


{-| An error type indicating that the two given vertices have the same position.
-}
type Error vertex
    = CoincidentVertices vertex vertex


{-| All the details about a particular face of a Delaunay triangulation:

  - The three input vertices it is formed from
  - The `Triangle2d` defining its shape
  - The circumcircle of that triangle

-}
type alias Face vertex =
    { vertices : ( vertex, vertex, vertex )
    , triangle : Triangle2d
    , circumcircle : Circle2d
    }


{-| An empty Delaunay triangulation with no vertices or faces.
-}
empty : DelaunayTriangulation2d vertex
empty =
    Types.EmptyDelaunayTriangulation2d


{-| Construct a Delaunay triangulation from an array of points. The points must
all be distinct; if any two points are equal, you will get an `Err
CoincidentVertices`.

Note that if all points are collinear, then the resulting triangulation will
be empty (have no faces).

-}
fromPoints : Array Point2d -> Result (Error Point2d) (DelaunayTriangulation2d Point2d)
fromPoints points =
    fromVerticesBy identity points


prependDelaunayVertex : (vertex -> Point2d) -> vertex -> List (DelaunayVertex vertex) -> List (DelaunayVertex vertex)
prependDelaunayVertex getPosition vertex accumulated =
    let
        index =
            case accumulated of
                [] ->
                    0

                previous :: _ ->
                    previous.index + 1

        newDelaunayVertex =
            { vertex = vertex
            , index = index
            , position = getPosition vertex
            }
    in
    newDelaunayVertex :: accumulated


delaunayVertexCoordinates : DelaunayVertex vertex -> ( Float, Float )
delaunayVertexCoordinates delaunayVertex =
    Point2d.coordinates delaunayVertex.position


checkDistinct : List (DelaunayVertex vertex) -> Result (Error vertex) ()
checkDistinct sortedDelaunayVertices =
    case sortedDelaunayVertices of
        [] ->
            Ok ()

        first :: rest ->
            checkDistinctHelp first rest


checkDistinctHelp : DelaunayVertex vertex -> List (DelaunayVertex vertex) -> Result (Error vertex) ()
checkDistinctHelp previous sortedDelaunayVertices =
    case sortedDelaunayVertices of
        [] ->
            Ok ()

        first :: rest ->
            if previous.position == first.position then
                Err (CoincidentVertices previous.vertex first.vertex)

            else
                checkDistinctHelp first rest


collectDelaunayVertices : (vertex -> Point2d) -> Array vertex -> Result (Error vertex) (List (DelaunayVertex vertex))
collectDelaunayVertices getPosition givenVertices =
    let
        allDelaunayVertices =
            Array.foldl (prependDelaunayVertex getPosition) [] givenVertices

        sortedDelaunayVertices =
            allDelaunayVertices |> List.sortBy delaunayVertexCoordinates
    in
    case checkDistinct sortedDelaunayVertices of
        Ok () ->
            Ok sortedDelaunayVertices

        Err coincidentVertices ->
            Err coincidentVertices


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


{-| Construct a Delaunay triangulation from an array of vertices of arbitrary
type, by supplying a function that returns the position of each vertex as a
`Point2d`. For example, if you had

    types alias Vertex =
        { position = Point2d
        , color = String
        }

and

    vertices : Array Vertex
    vertices =
        ...

then you would use

    DelaunayTriangulation2d.fromVerticesBy .position vertices

The vertices must all be distinct; if any two have the same position, you will
get an `Err CoincidentVertices`.

Note that if all vertices are collinear, then the resulting triangulation will
be empty (have no faces).

-}
fromVerticesBy : (vertex -> Point2d) -> Array vertex -> Result (Error vertex) (DelaunayTriangulation2d vertex)
fromVerticesBy getPosition givenVertices =
    case collectDelaunayVertices getPosition givenVertices of
        Ok delaunayVertices ->
            case delaunayVertices of
                firstDelaunayVertex :: remainingDelaunayVertices ->
                    let
                        initialFaces =
                            createInitialFaces firstDelaunayVertex

                        faces_ =
                            List.foldl addVertex initialFaces remainingDelaunayVertices
                    in
                    Ok <|
                        Types.DelaunayTriangulation2d
                            { vertices = givenVertices
                            , delaunayVertices = delaunayVertices
                            , faces = faces_
                            }

                [] ->
                    Ok Types.EmptyDelaunayTriangulation2d

        Err err ->
            Err err


{-| Add a new point into an existing Delaunay triangulation. It must not be
equal to any existing point; if it is, you will get an `Err CoincidentVertices`.
-}
insertPoint : Point2d -> DelaunayTriangulation2d Point2d -> Result (Error Point2d) (DelaunayTriangulation2d Point2d)
insertPoint point delaunayTriangulation =
    insertVertexBy identity point delaunayTriangulation


checkForCoincidentVertex : vertex -> Point2d -> List (DelaunayVertex vertex) -> Result (Error vertex) ()
checkForCoincidentVertex vertex point delaunayVertices =
    case delaunayVertices of
        [] ->
            Ok ()

        firstDelaunayVertex :: remainingDelaunayVertices ->
            if point == firstDelaunayVertex.position then
                Err (CoincidentVertices firstDelaunayVertex.vertex vertex)

            else
                checkForCoincidentVertex vertex point remainingDelaunayVertices


{-| Add a new vertex into an existing Delaunay triangulation, by supplying a
function to get the position of the vertex. The vertex must not have the same
position as any existing vertex; if it is, you will get an `Err
CoincidentVertices`.
-}
insertVertexBy : (vertex -> Point2d) -> vertex -> DelaunayTriangulation2d vertex -> Result (Error vertex) (DelaunayTriangulation2d vertex)
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
            Ok <|
                Types.DelaunayTriangulation2d
                    { vertices = Array.repeat 1 vertex
                    , faces = createInitialFaces initialDelaunayVertex
                    , delaunayVertices = [ initialDelaunayVertex ]
                    }

        Types.DelaunayTriangulation2d current ->
            case checkForCoincidentVertex vertex position current.delaunayVertices of
                Ok () ->
                    let
                        newDelaunayVertex =
                            { vertex = vertex
                            , position = position
                            , index = Array.length current.vertices
                            }
                    in
                    Ok <|
                        Types.DelaunayTriangulation2d
                            { vertices = current.vertices |> Array.push vertex
                            , delaunayVertices =
                                newDelaunayVertex :: current.delaunayVertices
                            , faces = addVertex newDelaunayVertex current.faces
                            }

                Err coincidentVertices ->
                    Err coincidentVertices


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


{-| Convert a Delaunay triangulation to a [`TriangularMesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/TriangularMesh#TriangularMesh).
Complexity: O(n).
-}
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


{-| Get all triangles in a given Delaunay triangulation;

    DelaunayTriangulation2d.triangles triangulation

is equivalent to

    DelaunayTriangulation2d.faces triangulation
        |> List.map .triangle

but somewhat more efficient. Complexity: O(n).

-}
triangles : DelaunayTriangulation2d vertex -> List Triangle2d
triangles delaunayTriangulation =
    collectFaces getTriangle delaunayTriangulation


getCircumcircle : DelaunayVertex vertex -> DelaunayVertex vertex -> DelaunayVertex vertex -> Circle2d -> Circle2d
getCircumcircle firstVertex secondVertex thirdVertex circumcircle =
    circumcircle


{-| Get all circumcircles in a given Delaunay triangulation;

    DelaunayTriangulation2d.circumcircles triangulation

is equivalent to

    DelaunayTriangulation2d.faces triangulation
        |> List.map .circumcircle

but somewhat more efficient. Complexity: O(n).

-}
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


{-| Get a list of all `Face`s in a given Delaunay triangulation. Complexity:
O(n).
-}
faces : DelaunayTriangulation2d vertex -> List (Face vertex)
faces delaunayTriangulation =
    collectFaces getFace delaunayTriangulation


{-| Get the vertices of a Delaunay triangulation. If the triangulation was
constructed by calling `fromPoints` or `fromVerticesBy`, then the returned
vertex array will simply be the array that was passed in. If any vertices were
added using `insertPoint` or `insertVertexBy`, then they will be appended to
the end of the array. This is a simple accessor, so complexity is O(1).
-}
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
