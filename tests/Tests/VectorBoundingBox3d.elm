module Tests.VectorBoundingBox3d exposing (arithmetic, length)

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
import Vector3d exposing (Vector3d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


boxAndContainedVector : Generator ( VectorBoundingBox3d Meters coordinates, Vector3d Meters coordinates )
boxAndContainedVector =
    Random.vectorBoundingBox3d
        |> Random.andThen
            (\box ->
                VectorBoundingBox3d.randomVector box
                    |> Random.map (\containedVector -> ( box, containedVector ))
            )


length : Test
length =
    Test.check "length" boxAndContainedVector <|
        \( box, vector ) ->
            Vector3d.length vector
                |> Expect.quantityContainedIn (VectorBoundingBox3d.length box)


testFloatOperation :
    String
    -> (Float -> Vector3d Meters coordinates -> Vector3d Meters coordinates)
    -> (Float -> VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d Meters coordinates)
    -> Test
testFloatOperation description vectorFunction boundingBoxFunction =
    Test.check5 description
        Random.scale
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\scalar boundingBox u v w ->
            let
                vector =
                    VectorBoundingBox3d.interpolate boundingBox u v w
            in
            vectorFunction scalar vector
                |> Expect.vector3dContainedIn
                    (boundingBoxFunction scalar boundingBox)
        )


testQuantityOperation :
    String
    -> (Quantity Float Unitless -> Vector3d Meters coordinates -> Vector3d resultUnits coordinates)
    -> (Quantity Float Unitless -> VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d resultUnits coordinates)
    -> Test
testQuantityOperation description vectorFunction boundingBoxFunction =
    Test.check5 description
        Random.unitlessQuantity
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\quantity boundingBox u v w ->
            let
                vector =
                    VectorBoundingBox3d.interpolate boundingBox u v w
            in
            vectorFunction quantity vector
                |> Expect.vector3dContainedIn
                    (boundingBoxFunction quantity boundingBox)
        )


testIntervalOperation :
    String
    -> (Quantity Float Unitless -> Vector3d Meters coordinates -> Vector3d resultUnits coordinates)
    -> (Interval Float Unitless -> VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d resultUnits coordinates)
    -> Test
testIntervalOperation description vectorFunction boundingBoxFunction =
    Test.check6 description
        Random.unitlessInterval
        Random.parameterValue
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\interval t boundingBox u v w ->
            let
                quantity =
                    Interval.interpolate interval t

                vector =
                    VectorBoundingBox3d.interpolate boundingBox u v w
            in
            vectorFunction quantity vector
                |> Expect.vector3dContainedIn
                    (boundingBoxFunction interval boundingBox)
        )


testUnaryOperation :
    String
    -> (Vector3d Meters coordinates -> Vector3d resultUnits coordinates)
    -> (VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d resultUnits coordinates)
    -> Test
testUnaryOperation description vectorFunction boundingBoxFunction =
    Test.check4 description
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\boundingBox u v w ->
            let
                vector =
                    VectorBoundingBox3d.interpolate boundingBox u v w
            in
            vectorFunction vector |> Expect.vector3dContainedIn (boundingBoxFunction boundingBox)
        )


testBinaryOperation :
    String
    -> (Vector3d Meters coordinates -> Vector3d Meters coordinates -> Vector3d resultUnits coordinates)
    -> (Vector3d Meters coordinates -> VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d resultUnits coordinates)
    -> Test
testBinaryOperation description vectorFunction boundingBoxFunction =
    Test.check5 description
        Random.vector3d
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\firstVector boundingBox u v w ->
            let
                secondVector =
                    VectorBoundingBox3d.interpolate boundingBox u v w
            in
            vectorFunction firstVector secondVector
                |> Expect.vector3dContainedIn (boundingBoxFunction firstVector boundingBox)
        )


testBinaryBoundingBoxOperation :
    String
    -> (Vector3d Meters coordinates -> Vector3d Meters coordinates -> Vector3d resultUnits coordinates)
    -> (VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d Meters coordinates -> VectorBoundingBox3d resultUnits coordinates)
    -> Test
testBinaryBoundingBoxOperation description vectorFunction boundingBoxFunction =
    Test.check8 description
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        Random.vectorBoundingBox3d
        Random.parameterValue
        Random.parameterValue
        Random.parameterValue
        (\firstBoundingBox u1 v1 w1 secondBoundingBox u2 v2 w2 ->
            let
                firstVector =
                    VectorBoundingBox3d.interpolate firstBoundingBox u1 v1 w1

                secondVector =
                    VectorBoundingBox3d.interpolate secondBoundingBox u2 v2 w2
            in
            vectorFunction firstVector secondVector
                |> Expect.vector3dContainedIn (boundingBoxFunction firstBoundingBox secondBoundingBox)
        )


arithmetic : Test
arithmetic =
    Test.describe "Arithmetic"
        [ testFloatOperation "multiplyBy" Vector3d.scaleBy VectorBoundingBox3d.multiplyBy
        , testUnaryOperation "half" Vector3d.half VectorBoundingBox3d.half
        , testUnaryOperation "twice" Vector3d.twice VectorBoundingBox3d.twice
        , testBinaryOperation "plus" Vector3d.plus VectorBoundingBox3d.plus
        , testBinaryBoundingBoxOperation "plusBoundingBox" Vector3d.plus VectorBoundingBox3d.plusBoundingBox
        , testBinaryOperation "minus" Vector3d.minus VectorBoundingBox3d.minus
        , testBinaryBoundingBoxOperation "minusBoundingBox" Vector3d.minus VectorBoundingBox3d.minusBoundingBox
        , testQuantityOperation "times" Vector3d.times VectorBoundingBox3d.times
        , testQuantityOperation "product" Vector3d.product VectorBoundingBox3d.product
        , testQuantityOperation "timesUnitless" Vector3d.timesUnitless VectorBoundingBox3d.timesUnitless
        , testIntervalOperation "timesInterval" Vector3d.times VectorBoundingBox3d.timesInterval
        , testIntervalOperation "timesUnitlessInterval" Vector3d.timesUnitless VectorBoundingBox3d.timesUnitlessInterval
        ]
