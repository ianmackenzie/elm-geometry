module DelaunayTriangulation2d
    exposing
        ( DelaunayTriangulation2d
        , empty
        , fromPoints
        , fromVerticesBy
        , insert
        , toMesh
        )


type DelaunayTriangulation2d vertex
    = EmptyDelaunayTriangulation
    | DelaunayTriangulation
        { verticesInReverseOrder : List vertex
        , numVertices : Int
        , faces : List Face
        }
