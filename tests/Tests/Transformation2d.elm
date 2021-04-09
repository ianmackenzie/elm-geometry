module Tests.Transformation2d exposing (..)

import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length
import Point2d
import Quantity exposing (zero)
import Test exposing (Test)
import Transformation2d
import Triangle2d
import Vector2d


sanityChecks =
    Test.describe "sanity checks"
        [ Test.fuzz2 Fuzz.point2d Fuzz.vector2d "translateBy" <|
            \point vec ->
                Point2d.translateBy vec point
                    |> Expect.point2d (Point2d.apply (Transformation2d.translateBy vec) point)
        , Test.fuzz3 Fuzz.point2d Fuzz.scale Fuzz.point2d "scaleAbout" <|
            \point k center ->
                Point2d.scaleAbout center k point
                    |> Expect.point2d (Point2d.apply (Transformation2d.scaleAbout center k) point)
        ]
