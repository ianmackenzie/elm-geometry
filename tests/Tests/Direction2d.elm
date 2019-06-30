module Tests.Direction2d exposing
    ( angleFromAndEqualWithinAreConsistent
    , angleFromAndRotateByAreConsistent
    , fromAngleIsConsistentWithAngleFrom
    , orthonormalizeProducesValidFrameBasis
    , orthonormalizingParallelVectorsReturnsNothing
    , perpendicularToIsConsistentWithRotateBy
    , rotateClockwiseIsConsistentWithRotateBy
    , rotateCounterclockwiseIsConsistentWithRotateBy
    )

import Angle
import Direction2d
import Expect
import Frame2d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import Point2d
import Quantity
import Test exposing (Test)
import Vector2d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Quantity.abs
                        (Direction2d.angleFrom firstDirection secondDirection)

                tolerance =
                    angle |> Quantity.plus (Angle.radians 1.0e-12)
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction2d.equalWithin tolerance
                    firstDirection
                    secondDirection
                )
        )


angleFromAndRotateByAreConsistent : Test
angleFromAndRotateByAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and rotateBy are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction2d.angleFrom firstDirection secondDirection
            in
            firstDirection
                |> Direction2d.rotateBy angle
                |> Expect.direction2d secondDirection
        )


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple ( Fuzz.vector2d, Fuzz.vector2d ))
        "orthonormalize produces a valid frame basis"
        (\( xVector, yVector ) ->
            case Direction2d.orthonormalize xVector yVector of
                Just ( xDirection, yDirection ) ->
                    Expect.validFrame2d
                        (Frame2d.unsafe
                            { originPoint = Point2d.origin
                            , xDirection = xDirection
                            , yDirection = yDirection
                            }
                        )

                Nothing ->
                    let
                        crossProduct =
                            xVector |> Vector2d.cross yVector
                    in
                    Expect.approximately Quantity.zero crossProduct
        )


orthonormalizingParallelVectorsReturnsNothing : Test
orthonormalizingParallelVectorsReturnsNothing =
    Test.test "orthonormalizing parallel vectors returns Nothing"
        (\() ->
            let
                xVector =
                    Vector2d.fromTuple meters ( 1, 2 )

                yVector =
                    Vector2d.fromTuple meters ( -3, -6 )
            in
            Expect.equal Nothing (Direction2d.orthonormalize xVector yVector)
        )


perpendicularToIsConsistentWithRotateBy : Test
perpendicularToIsConsistentWithRotateBy =
    Test.fuzz Fuzz.direction2d
        "perpendicularTo is consistent with rotateBy"
        (\direction ->
            Direction2d.perpendicularTo direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees 90) direction)
        )


rotateClockwiseIsConsistentWithRotateBy : Test
rotateClockwiseIsConsistentWithRotateBy =
    Test.fuzz Fuzz.direction2d
        "rotateClockwise is consistent with rotateBy"
        (\direction ->
            Direction2d.rotateClockwise direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees -90) direction)
        )


rotateCounterclockwiseIsConsistentWithRotateBy : Test
rotateCounterclockwiseIsConsistentWithRotateBy =
    Test.fuzz Fuzz.direction2d
        "rotateCounterclockwise is consistent with rotateBy"
        (\direction ->
            Direction2d.rotateCounterclockwise direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees 90) direction)
        )


fromAngleIsConsistentWithAngleFrom : Test
fromAngleIsConsistentWithAngleFrom =
    Test.fuzz Fuzz.direction2d
        "fromAngle is consistent with angleFrom"
        (Expect.roundTrip Expect.direction2d
            (Direction2d.angleFrom Direction2d.x >> Direction2d.fromAngle)
        )
        )
