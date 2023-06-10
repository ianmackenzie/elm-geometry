module Tests.Vector2d exposing
    ( components
    , dotProductWithSelfIsSquaredLength
    , mirrorAcrossNegatesPerpendicularComponent
    , mirrorAcrossPreservesParallelComponent
    , perpendicularVectorIsPerpendicular
    , rotateByPreservesLength
    , rotateByRotatesByTheCorrectAngle
    , sum
    , vectorScaling
    )

import Axis2d
import Direction2d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Quantity
import Random
import Test exposing (Test)
import Test.Check as Test
import Vector2d


perpendicularVectorIsPerpendicular : Test
perpendicularVectorIsPerpendicular =
    Test.check "perpendicularTo actually returns a perpendicular vector"
        Random.vector2d
        (\vector ->
            vector
                |> Vector2d.perpendicularTo
                |> Vector2d.dot vector
                |> Expect.quantity Quantity.zero
        )


dotProductWithSelfIsSquaredLength : Test
dotProductWithSelfIsSquaredLength =
    Test.check "Dot product of a vector with itself is its squared length"
        Random.vector2d
        (\vector ->
            (vector |> Vector2d.dot vector)
                |> Expect.quantity
                    (Quantity.squared (Vector2d.length vector))
        )


rotateByPreservesLength : Test
rotateByPreservesLength =
    Test.check2 "Rotating a vector preserves its length"
        Random.vector2d
        Random.angle
        (\vector angle ->
            Vector2d.rotateBy angle vector
                |> Vector2d.length
                |> Expect.quantity (Vector2d.length vector)
        )


rotateByRotatesByTheCorrectAngle : Test
rotateByRotatesByTheCorrectAngle =
    Test.check2 "Rotating a vector rotates by the correct angle"
        Random.vector2d
        Random.angle
        (\vector angle ->
            let
                rotatedVector =
                    Vector2d.rotateBy angle vector
            in
            if vector == Vector2d.zero then
                rotatedVector |> Expect.vector2d vector

            else
                let
                    direction =
                        Vector2d.direction vector

                    rotatedDirection =
                        Vector2d.direction (Vector2d.rotateBy angle vector)

                    measuredAngle =
                        Maybe.map2 Direction2d.angleFrom direction rotatedDirection
                            |> Maybe.withDefault Quantity.zero
                in
                Expect.angle angle measuredAngle
        )


mirrorAcrossPreservesParallelComponent : Test
mirrorAcrossPreservesParallelComponent =
    Test.check2 "Mirroring a vector across an axis preserves component parallel to the axis"
        Random.vector2d
        Random.axis2d
        (\vector axis ->
            let
                parallelComponent =
                    Vector2d.componentIn (Axis2d.direction axis)
            in
            vector
                |> Vector2d.mirrorAcross axis
                |> parallelComponent
                |> Expect.quantity (parallelComponent vector)
        )


mirrorAcrossNegatesPerpendicularComponent : Test
mirrorAcrossNegatesPerpendicularComponent =
    Test.check2 "Mirroring a vector across an axis negates component perpendicular to the axis"
        Random.vector2d
        Random.axis2d
        (\vector axis ->
            let
                perpendicularDirection =
                    Direction2d.perpendicularTo (Axis2d.direction axis)

                perpendicularComponent =
                    Vector2d.componentIn perpendicularDirection
            in
            vector
                |> Vector2d.mirrorAcross axis
                |> perpendicularComponent
                |> Expect.quantity
                    (Quantity.negate (perpendicularComponent vector))
        )


components : Test
components =
    Test.check "components and xComponent/yComponent are consistent" Random.vector2d <|
        \vector ->
            Expect.all
                [ Tuple.first >> Expect.quantity (Vector2d.xComponent vector)
                , Tuple.second >> Expect.quantity (Vector2d.yComponent vector)
                ]
                (Vector2d.components vector)


sum : Test
sum =
    Test.check "sum is consistent with plus" (Random.smallList Random.vector2d) <|
        \vectors ->
            Vector2d.sum vectors
                |> Expect.vector2d
                    (List.foldl Vector2d.plus Vector2d.zero vectors)


vectorScaling : Test
vectorScaling =
    Test.describe "scaling vectors"
        [ Test.check "scaling a zero vector results in a zero vector" Random.length <|
            \len ->
                Expect.equal Vector2d.zero (Vector2d.scaleTo len Vector2d.zero)
        , Test.check "scaleTo has a consistent length" (Random.pair Random.length Random.vector2d) <|
            \( scale, vector ) ->
                if vector == Vector2d.zero then
                    Vector2d.scaleTo scale vector
                        |> Expect.equal Vector2d.zero

                else
                    Vector2d.scaleTo scale vector
                        |> Vector2d.length
                        |> Expect.quantity (Quantity.abs scale)
        , Test.check "normalize has a consistent length" Random.vector2d <|
            \vector ->
                if vector == Vector2d.zero then
                    Vector2d.normalize vector
                        |> Expect.equal Vector2d.zero

                else
                    Vector2d.normalize vector
                        |> Vector2d.length
                        |> Expect.quantity (Quantity.float 1)
        ]
