module Tests.VectorBoundingBox2d exposing (arithmetic, length)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Test exposing (Test)
import Test.Check as Test
import Vector2d exposing (Vector2d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)


boxAndContainedVector : Generator ( VectorBoundingBox2d Meters coordinates, Vector2d Meters coordinates )
boxAndContainedVector =
    Random.vectorBoundingBox2d
        |> Random.andThen
            (\box ->
                VectorBoundingBox2d.randomVector box
                    |> Random.map (\containedVector -> ( box, containedVector ))
            )


length : Test
length =
    Test.check "length" boxAndContainedVector <|
        \( box, vector ) ->
            Vector2d.length vector
                |> Expect.quantityContainedIn (VectorBoundingBox2d.length box)


testFloatOperation :
    String
    -> (Float -> Vector2d Meters coordinates -> Vector2d Meters coordinates)
    -> (Float -> VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d Meters coordinates)
    -> Test
testFloatOperation description vectorFunction boundingBoxFunction =
    Test.check4 description
        Random.scale
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\scalar boundingBox u v ->
            let
                vector =
                    VectorBoundingBox2d.interpolate boundingBox u v
            in
            vectorFunction scalar vector
                |> Expect.vector2dContainedIn
                    (boundingBoxFunction scalar boundingBox)
        )


testQuantityOperation :
    String
    -> (Quantity Float Unitless -> Vector2d Meters coordinates -> Vector2d resultUnits coordinates)
    -> (Quantity Float Unitless -> VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d resultUnits coordinates)
    -> Test
testQuantityOperation description vectorFunction boundingBoxFunction =
    Test.check4 description
        Random.unitlessQuantity
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\quantity boundingBox u v ->
            let
                vector =
                    VectorBoundingBox2d.interpolate boundingBox u v
            in
            vectorFunction quantity vector
                |> Expect.vector2dContainedIn
                    (boundingBoxFunction quantity boundingBox)
        )


testIntervalOperation :
    String
    -> (Quantity Float Unitless -> Vector2d Meters coordinates -> Vector2d resultUnits coordinates)
    -> (Interval Float Unitless -> VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d resultUnits coordinates)
    -> Test
testIntervalOperation description vectorFunction boundingBoxFunction =
    Test.check5 description
        Random.unitlessInterval
        Random.parameterValue
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\interval t boundingBox u v ->
            let
                quantity =
                    Interval.interpolate interval t

                vector =
                    VectorBoundingBox2d.interpolate boundingBox u v
            in
            vectorFunction quantity vector
                |> Expect.vector2dContainedIn
                    (boundingBoxFunction interval boundingBox)
        )


testUnaryOperation :
    String
    -> (Vector2d Meters coordinates -> Vector2d resultUnits coordinates)
    -> (VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d resultUnits coordinates)
    -> Test
testUnaryOperation description vectorFunction boundingBoxFunction =
    Test.check3 description
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\boundingBox u v ->
            let
                vector =
                    VectorBoundingBox2d.interpolate boundingBox u v
            in
            vectorFunction vector |> Expect.vector2dContainedIn (boundingBoxFunction boundingBox)
        )


testBinaryOperation :
    String
    -> (Vector2d Meters coordinates -> Vector2d Meters coordinates -> Vector2d resultUnits coordinates)
    -> (Vector2d Meters coordinates -> VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d resultUnits coordinates)
    -> Test
testBinaryOperation description vectorFunction boundingBoxFunction =
    Test.check4 description
        Random.vector2d
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\firstVector boundingBox u v ->
            let
                secondVector =
                    VectorBoundingBox2d.interpolate boundingBox u v
            in
            vectorFunction firstVector secondVector
                |> Expect.vector2dContainedIn (boundingBoxFunction firstVector boundingBox)
        )


testBinaryBoundingBoxOperation :
    String
    -> (Vector2d Meters coordinates -> Vector2d Meters coordinates -> Vector2d resultUnits coordinates)
    -> (VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d Meters coordinates -> VectorBoundingBox2d resultUnits coordinates)
    -> Test
testBinaryBoundingBoxOperation description vectorFunction boundingBoxFunction =
    Test.check6 description
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        Random.vectorBoundingBox2d
        Random.parameterValue
        Random.parameterValue
        (\firstBoundingBox u1 v1 secondBoundingBox u2 v2 ->
            let
                firstVector =
                    VectorBoundingBox2d.interpolate firstBoundingBox u1 v1

                secondVector =
                    VectorBoundingBox2d.interpolate secondBoundingBox u2 v2
            in
            vectorFunction firstVector secondVector
                |> Expect.vector2dContainedIn (boundingBoxFunction firstBoundingBox secondBoundingBox)
        )


arithmetic : Test
arithmetic =
    Test.describe "Arithmetic"
        [ testFloatOperation "multiplyBy" Vector2d.scaleBy VectorBoundingBox2d.multiplyBy
        , testUnaryOperation "half" Vector2d.half VectorBoundingBox2d.half
        , testUnaryOperation "twice" Vector2d.twice VectorBoundingBox2d.twice
        , testBinaryOperation "plus" Vector2d.plus VectorBoundingBox2d.plus
        , testBinaryBoundingBoxOperation "plusBoundingBox" Vector2d.plus VectorBoundingBox2d.plusBoundingBox
        , testBinaryOperation "minus" Vector2d.minus VectorBoundingBox2d.minus
        , testBinaryBoundingBoxOperation "minusBoundingBox" Vector2d.minus VectorBoundingBox2d.minusBoundingBox
        , testQuantityOperation "times" Vector2d.times VectorBoundingBox2d.times
        , testQuantityOperation "product" Vector2d.product VectorBoundingBox2d.product
        , testQuantityOperation "timesUnitless" Vector2d.timesUnitless VectorBoundingBox2d.timesUnitless
        , testIntervalOperation "timesInterval" Vector2d.times VectorBoundingBox2d.timesInterval
        , testIntervalOperation "timesUnitlessInterval" Vector2d.timesUnitless VectorBoundingBox2d.timesUnitlessInterval
        ]
