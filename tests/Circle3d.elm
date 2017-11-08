module Circle3d
    exposing
        ( boundingBoxContainsCenter
        , jsonRoundTrips
        , throughPoints
        )

import Expect
import Generic
import OpenSolid.Axis3d as Axis3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Circle3d as Circle3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point3d as Point3d
import OpenSolid.Triangle3d as Triangle3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.circle3d
        Encode.circle3d
        Decode.circle3d


throughPoints : Test
throughPoints =
    Test.describe "throughPoints"
        [ Test.fuzz3 Fuzz.point3d
            Fuzz.point3d
            Fuzz.point3d
            "All given points lie on the circle constructed using `throughPoints`"
            (\p1 p2 p3 ->
                let
                    -- if the first three points are collinear it is not possible to construct a circle
                    -- that passes through them.
                    -- three points are collinear if the area of the triangle they form is zero.
                    isValidInput =
                        let
                            triangleArea =
                                Triangle3d.fromVertices ( p1, p2, p3 )
                                    |> Triangle3d.area
                        in
                        triangleArea > 1.0e-6

                    circle =
                        Circle3d.throughPoints ( p1, p2, p3 )

                    liesOnCircle point circle =
                        Point3d.distanceFrom point (Circle3d.centerPoint circle)
                            |> Expect.within (Expect.Absolute 1.0e-6) (Circle3d.radius circle)
                in
                case circle of
                    Just circle ->
                        Expect.all (List.map liesOnCircle [ p1, p2, p3 ]) circle

                    Nothing ->
                        if isValidInput then
                            Expect.fail "throughPoints returned Nothing on valid input"
                        else
                            Expect.pass
            )
        ]


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle3d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle3d.boundingBox circle

                centerPoint =
                    Circle3d.centerPoint circle
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox3d.contains centerPoint boundingBox)
        )
