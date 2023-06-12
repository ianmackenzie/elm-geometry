module Tests.Polygon2d exposing
    ( containsTest
    , convexHullContainsAllPoints
    , convexHullIsConvex
    , rectangleCentroidIsInTheCenter
    , regularTest
    , rotatingAroundCentroidKeepsCentroid
    , triangulationHasCorrectArea
    , triangulationHasCorrectNumberOfTriangles
    , triangulationHasCorrectWeightedCentroid
    )

import Angle
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length
import LineSegment2d
import Point2d
import Polygon2d
import Quantity exposing (zero)
import Random
import Rectangle2d
import Test exposing (Test)
import Test.Random as Test
import Triangle2d
import TriangularMesh
import Vector2d


{-| Implements the formula for area of a regular polygon.
-}
areaOfRegularNGon : Quantity.Quantity Float units -> Float -> Quantity.Quantity Float (Quantity.Squared units)
areaOfRegularNGon radius sides =
    Quantity.squared radius
        |> Quantity.multiplyBy sides
        |> Quantity.multiplyBy (Angle.sin (Angle.turns (1 / sides)))
        |> Quantity.divideBy 2


regularTest : Test
regularTest =
    Test.describe "regular"
        [ Test.check3 "A centroid of a regular polygon is in the center"
            Random.point2d
            Random.length
            (Random.int 3 300)
          <|
            \center radius sides ->
                Polygon2d.regular { centerPoint = center, circumradius = radius, numSides = sides }
                    |> Polygon2d.centroid
                    |> Expect.just (Expect.point2d center)
        , Test.check3 "The area matches what we would expect from a regular polygon"
            Random.point2d
            Random.length
            (Random.int 3 300)
          <|
            \center radius sides ->
                Polygon2d.regular { centerPoint = center, circumradius = radius, numSides = sides }
                    |> Polygon2d.area
                    |> Expect.quantity (areaOfRegularNGon radius (toFloat sides))
        , Test.test "sanity check" <|
            \() ->
                Polygon2d.regular
                    { centerPoint = Point2d.meters 0.5 0.5
                    , circumradius = Length.meters (sqrt 2 / 2)
                    , numSides = 4
                    }
                    |> Expect.polygon2d
                        (Polygon2d.singleLoop
                            [ Point2d.meters 1 0
                            , Point2d.meters 1 1
                            , Point2d.meters 0 1
                            , Point2d.meters 0 0
                            ]
                        )
        ]


convexHullIsConvex : Test
convexHullIsConvex =
    Test.check "The convex hull of a list of points is actually convex"
        (Random.smallList Random.point2d)
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
                    if List.all (Quantity.greaterThanOrEqualTo zero) crossProducts then
                        Expect.pass

                    else
                        Expect.fail "Edges should always turn counterclockwise"
        )


convexHullContainsAllPoints : Test
convexHullContainsAllPoints =
    Test.check "The convex hull of a list of points contains all of those points"
        (Random.smallList Random.point2d)
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
            if List.all isContained points then
                Expect.pass

            else
                Expect.fail "Convex hull should contain all points"
        )


simplePolygon =
    Polygon2d.singleLoop
        [ Point2d.meters 1 1
        , Point2d.meters 3 1
        , Point2d.meters 3 2
        , Point2d.meters 1 2
        ]


withHole =
    Polygon2d.withHoles
        [ [ Point2d.meters 1 1
          , Point2d.meters 1 2
          , Point2d.meters 2 2
          , Point2d.meters 2 1
          ]
        ]
        [ Point2d.meters 0 0
        , Point2d.meters 3 0
        , Point2d.meters 3 3
        , Point2d.meters 0 3
        ]


containsTest : Test
containsTest =
    Test.describe "contains"
        [ Test.test "inside" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.meters 2 1.5)
                    |> Expect.equal True
        , Test.test "boundary" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.meters 3 1.5)
                    |> Expect.equal True
        , Test.test "outside" <|
            \() ->
                simplePolygon
                    |> Polygon2d.contains (Point2d.meters 4 1.5)
                    |> Expect.equal False
        , Test.test "inside with hole" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.meters 2 2.5)
                    |> Expect.equal True
        , Test.test "boundary of hole" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.meters 2 2)
                    |> Expect.equal True
        , Test.test "outside (in the hole)" <|
            \() ->
                withHole
                    |> Polygon2d.contains (Point2d.meters 1.5 1.5)
                    |> Expect.equal False
        ]


triangulationHasCorrectArea : Test
triangulationHasCorrectArea =
    Test.check "The triangulation of a polygon has the same area as the polygon itself"
        Random.polygon2d
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
            triangleArea |> Expect.quantity polygonArea
        )


triangulationHasCorrectNumberOfTriangles : Test
triangulationHasCorrectNumberOfTriangles =
    Test.check "The triangulation of a polygon with n vertices and h holes has n + 2h - 2 triangles"
        Random.polygon2d
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


triangulationHasCorrectWeightedCentroid : Test
triangulationHasCorrectWeightedCentroid =
    Test.check "The centroid of the polygon before triangulation is the same as weighted centroid of all the resulting triangles"
        Random.polygon2d
        (\polygon ->
            let
                triangles =
                    Polygon2d.triangulate polygon
                        |> TriangularMesh.faceVertices
                        |> List.map Triangle2d.fromVertices

                centroidVectors =
                    triangles
                        |> List.map (Triangle2d.centroid >> Vector2d.from Point2d.origin)

                areas =
                    List.map Triangle2d.area triangles

                weightedCentroidVector =
                    Vector2d.sum (List.map2 Vector2d.product areas centroidVectors)
                        |> Vector2d.over (Quantity.sum areas)

                weightedCentroid =
                    Point2d.origin |> Point2d.translateBy weightedCentroidVector
            in
            case Polygon2d.centroid polygon of
                Just centroid ->
                    Expect.point2d weightedCentroid centroid

                Nothing ->
                    Expect.fail "Original polygon needs centroid"
        )


rotatingAroundCentroidKeepsCentroid : Test
rotatingAroundCentroidKeepsCentroid =
    Test.check2 "Rotating a polygon around its centroid keeps the centroid point"
        Random.polygon2d
        Random.angle
        (\polygon angle ->
            case Polygon2d.centroid polygon of
                Just centroid ->
                    case
                        polygon
                            |> Polygon2d.rotateAround centroid angle
                            |> Polygon2d.centroid
                    of
                        Just rotatedCentroid ->
                            Expect.point2d centroid rotatedCentroid

                        Nothing ->
                            Expect.fail "Rotated polygon needs centroid"

                Nothing ->
                    Expect.fail "Original polygon needs centroid"
        )


rectangleCentroidIsInTheCenter : Test
rectangleCentroidIsInTheCenter =
    Test.check "The centroid of rectangle is in the center point"
        Random.rectangle2d
        (\rectangle ->
            case
                rectangle
                    |> Rectangle2d.vertices
                    |> Polygon2d.singleLoop
                    |> Polygon2d.centroid
            of
                Just centroid ->
                    Expect.point2d centroid (Rectangle2d.centerPoint rectangle)

                Nothing ->
                    Expect.fail "Polygon needs centroid"
        )
