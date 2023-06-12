module Tests.Direction2d exposing
    ( angleFromAndEqualWithinAreConsistent
    , angleFromAndRotateByAreConsistent
    , components
    , equalWithin
    , fromAngleIsConsistentWithAngleFrom
    , mirrorNegatesAngleFromAxis
    , mirrorTwiceIsIdentity
    , orthonormalizeProducesValidFrameBasis
    , orthonormalizingParallelVectorsReturnsNothing
    , perpendicularToIsConsistentWithRotateBy
    , relativeToAndPlaceInAreInverses
    , rotateClockwiseIsConsistentWithRotateBy
    , rotateCounterclockwiseIsConsistentWithRotateBy
    , xComponentIsConsistentWithComponentIn
    , yComponentIsConsistentWithComponentIn
    )

import Angle
import Axis2d
import Direction2d
import Expect
import Frame2d
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (meters)
import Point2d
import Quantity
import Random
import Test exposing (Test)
import Test.Random as Test
import Vector2d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.check2 "angleFrom and equalWithin are consistent"
        Random.direction2d
        Random.direction2d
        (\firstDirection secondDirection ->
            let
                angle =
                    Quantity.abs
                        (Direction2d.angleFrom firstDirection secondDirection)

                tolerance =
                    angle |> Quantity.plus (Angle.radians 1.0e-12)
            in
            if Direction2d.equalWithin tolerance firstDirection secondDirection then
                Expect.pass

            else
                Expect.fail "Two directions should be equal to within the angle between them"
        )


angleFromAndRotateByAreConsistent : Test
angleFromAndRotateByAreConsistent =
    Test.check2 "angleFrom and rotateBy are consistent"
        Random.direction2d
        Random.direction2d
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
    Test.check "orthonormalize produces a valid frame basis"
        (Random.pair Random.vector2d Random.vector2d)
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
                    Expect.quantity Quantity.zero crossProduct
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
    Test.check "perpendicularTo is consistent with rotateBy"
        Random.direction2d
        (\direction ->
            Direction2d.perpendicularTo direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees 90) direction)
        )


rotateClockwiseIsConsistentWithRotateBy : Test
rotateClockwiseIsConsistentWithRotateBy =
    Test.check "rotateClockwise is consistent with rotateBy"
        Random.direction2d
        (\direction ->
            Direction2d.rotateClockwise direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees -90) direction)
        )


rotateCounterclockwiseIsConsistentWithRotateBy : Test
rotateCounterclockwiseIsConsistentWithRotateBy =
    Test.check "rotateCounterclockwise is consistent with rotateBy"
        Random.direction2d
        (\direction ->
            Direction2d.rotateCounterclockwise direction
                |> Expect.direction2d
                    (Direction2d.rotateBy (Angle.degrees 90) direction)
        )


fromAngleIsConsistentWithAngleFrom : Test
fromAngleIsConsistentWithAngleFrom =
    Test.check "fromAngle is consistent with angleFrom"
        Random.direction2d
        (\direction ->
            Direction2d.angleFrom Direction2d.x direction
                |> Direction2d.fromAngle
                |> Expect.direction2d direction
        )


fromAngleIsConsistentWithToAngle : Test
fromAngleIsConsistentWithToAngle =
    Test.check "fromAngle is consistent with toAngle"
        Random.direction2d
        (\direction ->
            Direction2d.toAngle direction
                |> Direction2d.fromAngle
                |> Expect.direction2d direction
        )


xComponentIsConsistentWithComponentIn : Test
xComponentIsConsistentWithComponentIn =
    Test.check "xComponent is consistent with componentIn"
        Random.direction2d
        (\direction ->
            Direction2d.xComponent direction
                |> Expect.exactly
                    (Direction2d.componentIn Direction2d.x direction)
        )


yComponentIsConsistentWithComponentIn : Test
yComponentIsConsistentWithComponentIn =
    Test.check "yComponent is consistent with componentIn"
        Random.direction2d
        (\direction ->
            Direction2d.yComponent direction
                |> Expect.exactly
                    (Direction2d.componentIn Direction2d.y direction)
        )


equalWithin : Test
equalWithin =
    Test.describe "equalWithin"
        [ Test.check "Rotation by 2 degrees"
            Random.direction2d
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 2) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 3) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 1) direction
                            >> Expect.equal False
                        ]
            )
        , Test.check "Rotation by 90 degrees"
            Random.direction2d
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 90) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 91) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 89) direction
                            >> Expect.equal False
                        ]
            )
        , Test.check "Rotation by 178 degrees"
            Random.direction2d
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 178) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 179) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 177) direction
                            >> Expect.equal False
                        ]
            )
        , Test.check2 "All directions are equal within 180 degrees"
            Random.direction2d
            Random.direction2d
            (\firstDirection secondDirection ->
                Direction2d.equalWithin (Angle.degrees 180.000001)
                    firstDirection
                    secondDirection
                    |> Expect.equal True
            )
        ]


mirrorTwiceIsIdentity : Test
mirrorTwiceIsIdentity =
    Test.check2 "Mirroring twice returns the original direction"
        Random.direction2d
        Random.axis2d
        (\direction axis ->
            direction
                |> Direction2d.mirrorAcross axis
                |> Direction2d.mirrorAcross axis
                |> Expect.direction2d direction
        )


mirrorNegatesAngleFromAxis : Test
mirrorNegatesAngleFromAxis =
    Test.check2 "Mirroring negates angle from axis"
        Random.direction2d
        Random.axis2d
        (\direction axis ->
            let
                axisDirection =
                    Axis2d.direction axis

                mirroredDirection =
                    direction |> Direction2d.mirrorAcross axis

                originalAngle =
                    direction |> Direction2d.angleFrom axisDirection

                mirroredAngle =
                    mirroredDirection |> Direction2d.angleFrom axisDirection
            in
            mirroredAngle |> Expect.angle (Quantity.negate originalAngle)
        )


relativeToAndPlaceInAreInverses : Test
relativeToAndPlaceInAreInverses =
    Test.check2 "relativeTo and placeIn are inverses"
        Random.direction2d
        Random.frame2d
        (\direction frame ->
            direction
                |> Direction2d.relativeTo frame
                |> Direction2d.placeIn frame
                |> Expect.direction2d direction
        )


components : Test
components =
    Test.check "components and xComponent/yComponent are consistent" Random.direction2d <|
        \direction ->
            Expect.all
                [ Tuple.first >> Expect.exactly (Direction2d.xComponent direction)
                , Tuple.second >> Expect.exactly (Direction2d.yComponent direction)
                ]
                (Direction2d.components direction)
