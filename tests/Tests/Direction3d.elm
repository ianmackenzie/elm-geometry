module Tests.Direction3d exposing
    ( angleFromAndEqualWithinAreConsistent
    , components
    , orthonormalizeFollowsOriginalVectors
    , orthonormalizeProducesValidFrameBasis
    , orthonormalizingCoplanarVectorsReturnsNothing
    , perpendicularDirectionIsPerpendicular
    , perpendicularDirectionIsValid
    , projectionIntoSketchPlaneWorksProperly
    )

import Angle
import Direction3d
import Expect
import Frame3d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import Point3d
import Quantity exposing (Quantity(..))
import SketchPlane3d
import Test exposing (Test)
import Vector3d
import Volume exposing (cubicMeters)


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction3d
        Fuzz.direction3d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction3d.angleFrom firstDirection secondDirection

                tolerance =
                    angle |> Quantity.plus (Angle.radians 1.0e-12)
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction3d.equalWithin tolerance
                    firstDirection
                    secondDirection
                )
        )


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple3 ( Fuzz.vector3d, Fuzz.vector3d, Fuzz.vector3d ))
        "orthonormalize produces a valid frame basis"
        (\( v1, v2, v3 ) ->
            let
                tripleProduct =
                    v1 |> Vector3d.cross v2 |> Vector3d.dot v3
            in
            if Quantity.abs tripleProduct |> Quantity.greaterThan (cubicMeters 1.0e-6) then
                case Direction3d.orthonormalize v1 v2 v3 of
                    Just ( xDirection, yDirection, zDirection ) ->
                        Expect.validFrame3d
                            (Frame3d.unsafe
                                { originPoint = Point3d.origin
                                , xDirection = xDirection
                                , yDirection = yDirection
                                , zDirection = zDirection
                                }
                            )

                    Nothing ->
                        Expect.fail "Could not orthonormalize valid set of vectors"

            else
                Expect.pass
        )


orthonormalizeFollowsOriginalVectors : Test
orthonormalizeFollowsOriginalVectors =
    Test.fuzz (Fuzz.tuple3 ( Fuzz.vector3d, Fuzz.vector3d, Fuzz.vector3d ))
        "orthonormalized directions follow original vectors properly"
        (\( v1, v2, v3 ) ->
            case Direction3d.orthonormalize v1 v2 v3 of
                Just directions ->
                    directions
                        |> Expect.all
                            [ \( xDirection, _, _ ) ->
                                Vector3d.componentIn xDirection v1
                                    |> Expect.quantityGreaterThan Quantity.zero
                            , \( _, yDirection, _ ) ->
                                Vector3d.componentIn yDirection v1
                                    |> Expect.quantity Quantity.zero
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v1
                                    |> Expect.quantity Quantity.zero
                            , \( _, yDirection, _ ) ->
                                Vector3d.componentIn yDirection v2
                                    |> Expect.quantityGreaterThan Quantity.zero
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v2
                                    |> Expect.quantity Quantity.zero
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v3
                                    |> Expect.quantityGreaterThan Quantity.zero
                            ]

                Nothing ->
                    Expect.pass
        )


orthonormalizingCoplanarVectorsReturnsNothing : Test
orthonormalizingCoplanarVectorsReturnsNothing =
    Test.test "orthonormalizing coplanar vectors returns Nothing"
        (\() ->
            let
                v1 =
                    Vector3d.fromTuple meters ( 1, 0, 0 )

                v2 =
                    Vector3d.fromTuple meters ( 2, 3, 0 )

                v3 =
                    Vector3d.fromTuple meters ( -1, 2, 0 )
            in
            Expect.equal Nothing (Direction3d.orthonormalize v1 v2 v3)
        )


perpendicularDirectionIsPerpendicular : Test
perpendicularDirectionIsPerpendicular =
    Test.fuzz Fuzz.direction3d
        "perpendicularTo returns a perpendicular direction"
        (\direction ->
            Direction3d.perpendicularTo direction
                |> Expect.direction3dPerpendicularTo direction
        )


perpendicularDirectionIsValid : Test
perpendicularDirectionIsValid =
    Test.fuzz Fuzz.direction3d
        "perpendicularTo returns a valid direction"
        (\direction ->
            Direction3d.perpendicularTo direction
                |> Expect.validDirection3d
        )


projectionIntoSketchPlaneWorksProperly : Test
projectionIntoSketchPlaneWorksProperly =
    Test.fuzz2
        Fuzz.direction3d
        Fuzz.sketchPlane3d
        "Projecting a direction into a sketch plane works properly"
        (\direction sketchPlane ->
            let
                normalDirection =
                    SketchPlane3d.normalDirection sketchPlane

                normalComponent =
                    Direction3d.componentIn normalDirection direction
            in
            case Direction3d.projectInto sketchPlane direction of
                Just direction2d ->
                    let
                        direction3d =
                            Direction3d.on sketchPlane direction2d
                    in
                    if abs normalComponent > 1.0e-6 then
                        let
                            crossProduct =
                                Direction3d.toVector direction
                                    |> Vector3d.cross
                                        (Direction3d.toVector direction3d)
                        in
                        crossProduct
                            |> Vector3d.componentIn normalDirection
                            |> Expect.quantity Quantity.zero

                    else if abs normalComponent < 1.0e-13 then
                        direction3d |> Expect.direction3d direction

                    else
                        Expect.pass

                Nothing ->
                    if normalComponent > 0 then
                        direction |> Expect.direction3d normalDirection

                    else
                        direction
                            |> Expect.direction3d
                                (Direction3d.reverse normalDirection)
        )


first : ( a, a, a ) -> a
first ( x, _, _ ) =
    x


second : ( a, a, a ) -> a
second ( _, y, _ ) =
    y


third : ( a, a, a ) -> a
third ( _, _, z ) =
    z


components : Test
components =
    Test.fuzz Fuzz.direction3d "components and xComponent etc. are consistent" <|
        \direction ->
            Expect.all
                [ first >> Expect.exactly (Direction3d.xComponent direction)
                , second >> Expect.exactly (Direction3d.yComponent direction)
                , third >> Expect.exactly (Direction3d.zComponent direction)
                ]
                (Direction3d.components direction)
