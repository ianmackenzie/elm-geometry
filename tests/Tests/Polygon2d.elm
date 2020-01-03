module Tests.Polygon2d exposing
    ( containsTest
    , convexHullContainsAllPoints
    , convexHullIsConvex
    , intersectionTest
    , triangulationHasCorrectArea
    , triangulationHasCorrectNumberOfTriangles
    )

import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import LineSegment2d
import Point2d
import Polygon2d
import Quantity exposing (zero)
import Test exposing (Test)
import Triangle2d
import TriangularMesh
import Vector2d


intersectionTest : Test
intersectionTest =
    Test.test "intersection" <|
        \() ->
            let
                polyA =
                    Polygon2d.with { outerLoop = [ Point2d.fromCoordinates ( 0, 60 ), Point2d.fromCoordinates ( 30, 30 ), Point2d.fromCoordinates ( 0, 30 ) ], innerLoops = [] }

                polyB =
                    Polygon2d.with { outerLoop = [ Point2d.fromCoordinates ( 0, 0 ), Point2d.fromCoordinates ( 60, 90 ), Point2d.fromCoordinates ( 60, 30 ) ], innerLoops = [] }

                resA =
                    Polygon2d.with
                        { outerLoop = [ Point2d.fromCoordinates ( 30, 30 ), Point2d.fromCoordinates ( 24, 36 ), Point2d.fromCoordinates ( 20, 30 ) ]
                        , innerLoops = []
                        }
            in
            Polygon2d.intersection polyB polyA
                |> Maybe.withDefault (Polygon2d.singleLoop [])
                |> Expect.polygon2d resA


convexHullIsConvex : Test
convexHullIsConvex =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "The convex hull of a list of points is actually convex"
        (\points ->
            let
                convexHull =
                    Polygon2d.convexHull points

                edgeVectors =
                    Polygon2d.edges convexHull |> List.map LineSegment2d.vector
            in
            case edgeVectors of
                [] ->
                    Expect.pass

                [ single ] ->
                    Expect.fail "Polygon should never have a single edge"

                first :: rest ->
                    let
                        crossProducts =
                            List.map2 (\v1 v2 -> v1 |> Vector2d.cross v2)
                                (first :: rest)
                                (rest ++ [ first ])
                    in
                    Expect.true "Edges should always turn counterclockwise" <|
                        List.all (Quantity.greaterThanOrEqualTo zero) crossProducts
        )


convexHullContainsAllPoints : Test
convexHullContainsAllPoints =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "The convex hull of a list of points contains all of those points"
        (\points ->
            let
                convexHull =
                    Polygon2d.convexHull points

                edges =
                    Polygon2d.edges convexHull

                isNonNegativeArea point edge =
                    let
                        ( p1, p2 ) =
                            LineSegment2d.endpoints edge

                        triangle =
                            Triangle2d.from point p1 p2
                    in
                    Triangle2d.counterclockwiseArea triangle
                        |> Quantity.greaterThanOrEqualTo zero

                isContained point =
                    List.all (\edge -> isNonNegativeArea point edge) edges
            in
            Expect.true "Convex hull should contain all points" <|
                List.all isContained points
        )


simplePolygon =
    Polygon2d.singleLoop
        [ Point2d.fromTuple meters ( 1, 1 )
        , Point2d.fromTuple meters ( 3, 1 )
        , Point2d.fromTuple meters ( 3, 2 )
        , Point2d.fromTuple meters ( 1, 2 )
        ]


withHole =
    Polygon2d.withHoles
        [ [ Point2d.fromTuple meters ( 1, 1 )
          , Point2d.fromTuple meters ( 1, 2 )
          , Point2d.fromTuple meters ( 2, 2 )
          , Point2d.fromTuple meters ( 2, 1 )
          ]
        ]
        [ Point2d.fromTuple meters ( 0, 0 )
        , Point2d.fromTuple meters ( 3, 0 )
        , Point2d.fromTuple meters ( 3, 3 )
        , Point2d.fromTuple meters ( 0, 3 )
        ]


containsTest : Test
containsTest =
    Test.describe "contains"
        [ Test.test "inside" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 2, 1.5 ))
                    |> Expect.equal True
        , Test.test "boundary" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 3, 1.5 ))
                    |> Expect.equal True
        , Test.test "outside" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 4, 1.5 ))
                    |> Expect.equal False
        , Test.test "inside with hole" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 2, 2.5 ))
                    |> Expect.equal True
        , Test.test "boundary of hole" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 2, 2 ))
                    |> Expect.equal True
        , Test.test "outside (in the hole)" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.fromTuple meters ( 1.5, 1.5 ))
                    |> Expect.equal False
        ]


triangulationHasCorrectArea : Test
triangulationHasCorrectArea =
    Test.fuzz Fuzz.polygon2d
        "The triangulation of a polygon has the same area as the polygon itself"
        (\polygon ->
            let
                polygonArea =
                    Polygon2d.area polygon

                triangles =
                    Polygon2d.triangulate polygon
                        |> TriangularMesh.faceVertices
                        |> List.map Triangle2d.fromVertices

                triangleArea =
                    Quantity.sum (List.map Triangle2d.area triangles)
            in
            triangleArea |> Expect.approximately polygonArea
        )


triangulationHasCorrectNumberOfTriangles : Test
triangulationHasCorrectNumberOfTriangles =
    Test.fuzz Fuzz.polygon2d
        "The triangulation of a polygon with n vertices and h holes has n + 2h - 2 triangles"
        (\polygon ->
            let
                innerLoops =
                    Polygon2d.innerLoops polygon

                numberOfVertices =
                    List.length (Polygon2d.outerLoop polygon)
                        + List.sum (List.map List.length innerLoops)

                numberOfHoles =
                    List.length innerLoops

                expectedNumberOfTriangles =
                    numberOfVertices + 2 * numberOfHoles - 2
            in
            polygon
                |> Polygon2d.triangulate
                |> TriangularMesh.faceVertices
                |> List.length
                |> Expect.equal expectedNumberOfTriangles
        )
