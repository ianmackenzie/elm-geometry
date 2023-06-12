module Tests.Circle2d exposing (boundingBoxContainsCenter, intersectsBoundingBox)

import BoundingBox2d
import Circle2d
import Expect
import Geometry.Random as Random
import Length
import Point2d
import Test exposing (Test, test)
import Test.Random as Test


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.check "A circle's bounding box contains its center point"
        Random.circle2d
        (\circle ->
            let
                boundingBox =
                    Circle2d.boundingBox circle

                centerPoint =
                    Circle2d.centerPoint circle
            in
            if BoundingBox2d.contains centerPoint boundingBox then
                Expect.pass

            else
                Expect.fail "Circle bounding box does not contain the center point"
        )


intersectsBoundingBox : Test
intersectsBoundingBox =
    let
        someBox x1 x2 y1 y2 =
            BoundingBox2d.fromExtrema
                { minX = Length.meters x1
                , maxX = Length.meters x2
                , minY = Length.meters y1
                , maxY = Length.meters y2
                }

        someCircle r center =
            Circle2d.withRadius (Length.meters r) center

        noIntersectionFound =
            "Expected an intersection to be found"

        unexpectedIntersection =
            "Expected no intersection to be found"
    in
    Test.describe "Intersection between a circle and a bounding box"
        [ test "Detects intersection when overlapping in both X and Y" <|
            \_ ->
                let
                    box =
                        someBox 1 5 1 5

                    circle =
                        someCircle 2 Point2d.origin
                in
                if Circle2d.intersectsBoundingBox box circle then
                    Expect.pass

                else
                    Expect.fail noIntersectionFound
        , test "Detects no intersection when not overlapping" <|
            \_ ->
                let
                    box =
                        someBox 20 22 30 40

                    circle =
                        Point2d.meters -20 -20
                            |> someCircle 5
                in
                if Circle2d.intersectsBoundingBox box circle then
                    Expect.fail unexpectedIntersection

                else
                    Expect.pass
        , test "Detects intersects when box and circle touch by exactly one pixel" <|
            \_ ->
                let
                    box =
                        someBox 1 1 0 0

                    circle =
                        someCircle 1 Point2d.origin
                in
                if Circle2d.intersectsBoundingBox box circle then
                    Expect.pass

                else
                    Expect.fail noIntersectionFound
        ]
