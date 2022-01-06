module Tests.Transformation2d exposing (suite)

import Direction2d exposing (Direction2d)
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, zero)
import Speed exposing (Speed)
import Test exposing (Test)
import Transformation2d exposing (Allowed, Transformation2d)
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


type Op coords
    = TranslateBy (Vector2d Meters coords)
    | TranslateIn (Direction2d coords) (Quantity Float Meters)
    | At Speed Speed
    | ScaleAbout (Point2d Meters coords) Float


opToTransformation : Op coords -> Transformation2d (Meters -> Meters) (coords -> coords) { restrictions | scale : Allowed }
opToTransformation op =
    case op of
        TranslateBy vec ->
            Transformation2d.translateBy vec

        TranslateIn dir dist ->
            Transformation2d.translateIn dir dist

        At rate1 rate2 ->
            Transformation2d.at_ rate1 |> Transformation2d.followedBy (Transformation2d.at rate2)

        ScaleAbout point k ->
            Transformation2d.scaleAbout point k


opToPoint : Op coords -> Point2d Meters coords -> Point2d Meters coords
opToPoint op =
    case op of
        TranslateBy vec ->
            Point2d.translateBy vec

        TranslateIn dir dist ->
            Point2d.translateIn dir dist

        At rate1 rate2 ->
            Point2d.at_ rate1 >> Point2d.at rate2

        ScaleAbout point k ->
            Point2d.scaleAbout point k


fuzzOp : Fuzzer (Op coords)
fuzzOp =
    Fuzz.oneOf
        [ Fuzz.map TranslateBy Fuzz.vector2d
        , Fuzz.map2 TranslateIn Fuzz.direction2d Fuzz.length
        , Fuzz.map2 At Fuzz.speed Fuzz.speed
        , Fuzz.map2 ScaleAbout Fuzz.point2d Fuzz.scale
        ]


suite =
    Test.describe "matches existing transformations for"
        [ Test.fuzz2 Fuzz.point2d fuzzOp "point" <|
            \point op ->
                opToPoint op point
                    |> Expect.point2d (Point2d.apply (opToTransformation op) point)

        , Test.fuzz2 Fuzz.point2d Fuzz.frame2d "relativeTo" <|
            \point frame ->
                Point2d.relativeTo frame point 
                |> Expect.point2d  (Point2d.apply (Transformation2d.relativeTo frame) point)
        ]
