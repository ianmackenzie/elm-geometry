module Tests.DelaunayTriangulation2d exposing
    ( allDelaunayTrianglesHaveNonzeroArea
    ,  delaunayTriangleContainsOnlyItsVertices
       --, emptyDelaunayMeansCollinearInput

    , failsOnCoincidentVertices
    )

import Array exposing (Array)
import Circle2d
import DelaunayTriangulation2d
import Direction3d
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import List.Extra
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import SketchPlane3d
import Test exposing (Test)
import Triangle2d
import Vector3d


uniquePoints : Fuzzer (Array Point2d)
uniquePoints =
    Fuzz.list Fuzz.point2d
        |> Fuzz.map (List.Extra.uniqueBy Point2d.coordinates)
        |> Fuzz.map Array.fromList


allDelaunayTrianglesHaveNonzeroArea : Test
allDelaunayTrianglesHaveNonzeroArea =
    let
        description =
            "The delaunay triangulation only produces triangles with non-zero area"

        expectation points =
            case DelaunayTriangulation2d.fromPoints points of
                Err _ ->
                    Expect.pass

                Ok triangulation ->
                    let
                        triangles =
                            DelaunayTriangulation2d.triangles triangulation
                    in
                    case List.filter (\triangle -> Triangle2d.area triangle <= 0) triangles of
                        [] ->
                            Expect.pass

                        x :: xs ->
                            Expect.fail ("DelaunayTriangulation2d produced a triangle with non-zero area: " ++ Debug.toString x)
    in
    Test.fuzz uniquePoints description expectation


delaunayTriangleContainsOnlyItsVertices : Test
delaunayTriangleContainsOnlyItsVertices =
    let
        description =
            "A delaunay triangle's circumcircle only contains its three vertices, no other points"

        expectation points =
            let
                check triangle =
                    case Triangle2d.circumcircle triangle of
                        Nothing ->
                            Err ("A delaunay triangle is degenerate: " ++ Debug.toString triangle)

                        Just circle ->
                            let
                                ( p1, p2, p3 ) =
                                    Triangle2d.vertices triangle

                                predicate point =
                                    Circle2d.contains point circle
                                        && (point /= p1)
                                        && (point /= p2)
                                        && (point /= p3)

                                containedPoints =
                                    Array.filter predicate points
                            in
                            if Array.isEmpty containedPoints then
                                Ok ()

                            else
                                Err ("A delaunay triangle circumcircle contains non-vertex points " ++ Debug.toString containedPoints)

                checkAll remainingTriangles =
                    case remainingTriangles of
                        [] ->
                            Expect.pass

                        triangle :: rest ->
                            case check triangle of
                                Ok _ ->
                                    checkAll rest

                                Err errorMessage ->
                                    Expect.fail errorMessage
            in
            case DelaunayTriangulation2d.fromPoints points of
                Err _ ->
                    Expect.pass

                Ok triangulation ->
                    checkAll (DelaunayTriangulation2d.triangles triangulation)
    in
    Test.fuzz uniquePoints description expectation



--emptyDelaunayMeansCollinearInput : Test
--emptyDelaunayMeansCollinearInput =
--    let
--        description =
--            "If a Delaunay triangulation is empty, then all input points must be collinear"
--        expectation points =
--            Expect.fail "TODO"
--    in
--    Test.fuzz uniquePoints description expectation


failsOnCoincidentVertices : Test
failsOnCoincidentVertices =
    let
        description =
            "Delaunay triangulation construction should fail when coincident vertices are given"

        expectation points =
            case points of
                [] ->
                    Expect.pass

                x :: xs ->
                    let
                        pointsWithDuplicate =
                            Array.fromList (x :: x :: xs)
                    in
                    DelaunayTriangulation2d.fromPoints pointsWithDuplicate
                        |> Expect.err
    in
    -- use normal `Fuzz.list`, more duplicates don't matter here
    Test.fuzz (Fuzz.list Fuzz.point2d) description expectation
