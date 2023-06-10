module Tests.Circle3d exposing (boundingBoxContainsCenter, throughPoints)

import Area
import BoundingBox3d
import Circle3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Point3d
import Quantity
import Test exposing (Test)
import Test.Check as Test
import Triangle3d


throughPoints : Test
throughPoints =
    Test.describe "throughPoints"
        [ Test.check3 "All given points lie on the circle constructed using `throughPoints`"
            Random.point3d
            Random.point3d
            Random.point3d
            (\p1 p2 p3 ->
                let
                    -- if the first three points are collinear it is not possible to construct a circle
                    -- that passes through them.
                    -- three points are collinear if the area of the triangle they form is zero.
                    isValidInput =
                        let
                            triangleArea =
                                Triangle3d.area (Triangle3d.from p1 p2 p3)
                        in
                        triangleArea |> Quantity.greaterThan (Area.squareMeters 1.0e-6)

                    maybeCircle =
                        Circle3d.throughPoints p1 p2 p3

                    liesOnCircle point circle =
                        Point3d.distanceFrom point (Circle3d.centerPoint circle)
                            |> Expect.quantity (Circle3d.radius circle)
                in
                case maybeCircle of
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
    Test.check "A circle's bounding box contains its center point"
        Random.circle3d
        (\circle ->
            let
                boundingBox =
                    Circle3d.boundingBox circle

                centerPoint =
                    Circle3d.centerPoint circle
            in
            if BoundingBox3d.contains centerPoint boundingBox then
                Expect.pass

            else
                Expect.fail "Circle bounding box does not contain the center point"
        )
