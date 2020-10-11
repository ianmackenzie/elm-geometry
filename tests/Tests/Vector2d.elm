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
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Quantity
import Test exposing (Test)
import Vector2d


perpendicularVectorIsPerpendicular : Test
perpendicularVectorIsPerpendicular =
    Test.fuzz Fuzz.vector2d
        "perpendicularTo actually returns a perpendicular vector"
        (\vector ->
            vector
                |> Vector2d.perpendicularTo
                |> Vector2d.dot vector
                |> Expect.quantity Quantity.zero
        )


dotProductWithSelfIsSquaredLength : Test
dotProductWithSelfIsSquaredLength =
    Test.fuzz Fuzz.vector2d
        "Dot product of a vector with itself is its squared length"
        (\vector ->
            (vector |> Vector2d.dot vector)
                |> Expect.quantity
                    (Quantity.squared (Vector2d.length vector))
        )


rotateByPreservesLength : Test
rotateByPreservesLength =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.angle
        "Rotating a vector preserves its length"
        (\vector angle ->
            Vector2d.rotateBy angle vector
                |> Vector2d.length
                |> Expect.quantity (Vector2d.length vector)
        )


rotateByRotatesByTheCorrectAngle : Test
rotateByRotatesByTheCorrectAngle =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.angle
        "Rotating a vector rotates by the correct angle"
        (\vector angle ->
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
    Test.fuzz2 Fuzz.vector2d
        Fuzz.axis2d
        "Mirroring a vector across an axis preserves component parallel to the axis"
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
    Test.fuzz2 Fuzz.vector2d
        Fuzz.axis2d
        "Mirroring a vector across an axis negates component perpendicular to the axis"
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
    Test.fuzz Fuzz.vector2d "components and xComponent/yComponent are consistent" <|
        \vector ->
            Expect.all
                [ Tuple.first >> Expect.quantity (Vector2d.xComponent vector)
                , Tuple.second >> Expect.quantity (Vector2d.yComponent vector)
                ]
                (Vector2d.components vector)


sum : Test
sum =
    Test.fuzz (Fuzz.list Fuzz.vector2d) "sum is consistent with plus" <|
        \vectors ->
            Vector2d.sum vectors
                |> Expect.vector2d
                    (List.foldl Vector2d.plus Vector2d.zero vectors)


vectorScaling : Test
vectorScaling =
    Test.describe "scaling vectors"
        [ Test.fuzz Fuzz.length "scaling a zero vector results in a zero vector" <|
            \len ->
                Expect.equal Vector2d.zero (Vector2d.scaleTo len Vector2d.zero)
        , Test.fuzz (Fuzz.tuple ( Fuzz.length, Fuzz.vector2d )) "scaleTo has a consistent length" <|
            \( scale, vector ) ->
                if vector == Vector2d.zero then
                    Vector2d.scaleTo scale vector
                        |> Expect.equal Vector2d.zero

                else
                    Vector2d.scaleTo scale vector
                        |> Vector2d.length
                        |> Expect.quantity (Quantity.abs scale)
        , Test.fuzz Fuzz.vector2d "normalize has a consistent length" <|
            \vector ->
                if vector == Vector2d.zero then
                    Vector2d.normalize vector
                        |> Expect.equal Vector2d.zero

                else
                    Vector2d.normalize vector
                        |> Vector2d.length
                        |> Expect.quantity (Quantity.float 1)
        ]
