module Tests.Polygon2d
    exposing
        ( convexHullContainsAllPoints
        , convexHullIsConvex
        , jsonRoundTrips
        )

import Expect
import Fuzz
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Fuzz as Fuzz
import LineSegment2d
import Polygon2d
import Test exposing (Test)
import Tests.Generic as Generic
import Triangle2d
import Vector2d


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polygon2d
        Encode.polygon2d
        Decode.polygon2d


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
                            List.map2 Vector2d.crossProduct
                                (first :: rest)
                                (rest ++ [ first ])

                        isNonNegative crossProduct =
                            crossProduct >= 0
                    in
                    Expect.true "Edges should always turn counterclockwise" <|
                        List.all isNonNegative crossProducts
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
                            Triangle2d.fromVertices ( point, p1, p2 )
                    in
                    Triangle2d.counterclockwiseArea triangle >= 0

                isContained point =
                    List.all (\edge -> isNonNegativeArea point edge) edges
            in
            Expect.true "Convex hull should contain all points" <|
                List.all isContained points
        )
