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
        (\direction ->
            Direction2d.angleFrom Direction2d.x direction
                |> Direction2d.fromAngle
                |> Expect.direction2d direction
        )


fromAngleIsConsistentWithToAngle : Test
fromAngleIsConsistentWithToAngle =
    Test.fuzz Fuzz.direction2d
        "fromAngle is consistent with toAngle"
        (\direction ->
            Direction2d.toAngle direction
                |> Direction2d.fromAngle
                |> Expect.direction2d direction
        )


xComponentIsConsistentWithComponentIn : Test
xComponentIsConsistentWithComponentIn =
    Test.fuzz Fuzz.direction2d
        "xComponent is consistent with componentIn"
        (\direction ->
            Direction2d.xComponent direction
                |> Expect.exactly
                    (Direction2d.componentIn Direction2d.x direction)
        )


yComponentIsConsistentWithComponentIn : Test
yComponentIsConsistentWithComponentIn =
    Test.fuzz Fuzz.direction2d
        "yComponent is consistent with componentIn"
        (\direction ->
            Direction2d.yComponent direction
                |> Expect.exactly
                    (Direction2d.componentIn Direction2d.y direction)
        )


equalWithin : Test
equalWithin =
    Test.describe "equalWithin"
        [ Test.fuzz Fuzz.direction2d
            "Rotation by 2 degrees"
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 2) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 3) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 1) direction
                            >> Expect.equal False
                        ]
            )
        , Test.fuzz Fuzz.direction2d
            "Rotation by 90 degrees"
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 90) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 91) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 89) direction
                            >> Expect.equal False
                        ]
            )
        , Test.fuzz Fuzz.direction2d
            "Rotation by 178 degrees"
            (\direction ->
                Direction2d.rotateBy (Angle.degrees 178) direction
                    |> Expect.all
                        [ Direction2d.equalWithin (Angle.degrees 179) direction
                            >> Expect.equal True
                        , Direction2d.equalWithin (Angle.degrees 177) direction
                            >> Expect.equal False
                        ]
            )
        , Test.fuzz2
            Fuzz.direction2d
            Fuzz.direction2d
            "All directions are equal within 180 degrees"
            (\firstDirection secondDirection ->
                Direction2d.equalWithin (Angle.degrees 180.000001)
                    firstDirection
                    secondDirection
                    |> Expect.equal True
            )
        ]


mirrorTwiceIsIdentity : Test
mirrorTwiceIsIdentity =
    Test.fuzz2
        Fuzz.direction2d
        Fuzz.axis2d
        "Mirroring twice returns the original direction"
        (\direction axis ->
            direction
                |> Direction2d.mirrorAcross axis
                |> Direction2d.mirrorAcross axis
                |> Expect.direction2d direction
        )


mirrorNegatesAngleFromAxis : Test
mirrorNegatesAngleFromAxis =
    Test.fuzz2
        Fuzz.direction2d
        Fuzz.axis2d
        "Mirroring negates angle from axis"
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
    Test.fuzz2
        Fuzz.direction2d
        Fuzz.frame2d
        "relativeTo and placeIn are inverses"
        (\direction frame ->
            direction
                |> Direction2d.relativeTo frame
                |> Direction2d.placeIn frame
                |> Expect.direction2d direction
        )


components : Test
components =
    Test.fuzz Fuzz.direction2d "components and xComponent/yComponent are consistent" <|
        \direction ->
            Expect.all
                [ Tuple.first >> Expect.exactly (Direction2d.xComponent direction)
                , Tuple.second >> Expect.exactly (Direction2d.yComponent direction)
                ]
                (Direction2d.components direction)
