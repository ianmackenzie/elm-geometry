module Tests.DelaunayTriangulation2d exposing (allDelaunayTrianglesHaveNonzeroArea, delaunayTriangleContainsOnlyItsVertices, emptyDelaunayMeansColinearInput, failsOnCoincidentVertices)

import DelaunayTriangulation2d
import Direction3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d
import Point3d
import SketchPlane3d
import Test exposing (Test)
import Vector3d


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
    Test.fuzz3 (Fuzz.listWithoutDuplicates Fuzz.point2d) description expectation


delaunayTriangleContainsOnlyItsVertices : Test
delaunayTriangleContainsOnlyItsVertices =
    let
        description =
            "A delaunay triangle's circumcircle only contains its three vertices, no other points"

        expectation points =
            let
                check triangle =
                    case Triangle2d.circumCircle triangle of
                        Nothing ->
                            Err ("A delaunay triangle is degenerate: " ++ Debug.toString triangle)

                        Just circle ->
                            let
                                ( p1, p2, p3 ) =
                                    Triangle.vertices triangle

                                predicate point =
                                    if point == p1 || point == p2 || point == p3 then
                                        not (Circle2d.contains point circle)

                                    else
                                        Circle2d.contains point circle
                            in
                            case List.filter predicate points of
                                [] ->
                                    Ok ()

                                x :: xs ->
                                    Err ("A delaunay triangle circumcircle contains a non-vertex point: " ++ Debug.toString x)

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
    Test.fuzz3 (Fuzz.listWithoutDuplicates Fuzz.point2d) description expectation


emptyDelaunayMeansColinearInput : Test
emptyDelaunayMeansColinearInput =
    let
        description =
            "If a Delaunay triangulation is empty, then all input points must be collinear"

        expectation points =
            Expect.fail "TODO"
    in
    Test.fuzz3 (Fuzz.listWithoutDuplicates Fuzz.point2d) description expectation


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
                            x :: x :: xs
                    in
                    DelaunayTriangulation2d.fromPoints pointsWithDuplicate
                        |> Expect.err
    in
    -- use normal `Fuzz.list`, more duplicates don't matter here
    Test.fuzz3 (Fuzz.list Fuzz.point2d) description expectation
