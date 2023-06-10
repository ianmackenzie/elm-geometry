module Tests.Sphere3d exposing
    ( FourPoints(..)
    , boundingBoxContainsCenter
    , mirrorAcross
    , properties
    , rotateAround
    , scaleAbout
    , sphereFromTuple
    , tetrahedronVolume
    , throughPointsCheck
    , throughPointsManual
    , translateBy
    , triangleArea
    , validTetrahedron
    , withRadius
    )

import Angle
import Area exposing (Area)
import Axis3d
import BoundingBox3d
import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters, meters)
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Cubed, Quantity, Squared, Unitless)
import Random
import Sphere3d exposing (Sphere3d)
import Test exposing (Test)
import Test.Check as Test
import Triangle3d
import Vector3d
import Volume exposing (Volume)



-- Helper methods


sphereFromTuple : ( ( Float, Float, Float ), Float ) -> Sphere3d Meters coordinates
sphereFromTuple ( centerPoint, radius ) =
    Sphere3d.withRadius (meters radius) (Point3d.fromTuple meters centerPoint)



-- Tests


withRadius : Test
withRadius =
    Test.describe "withRadius"
        [ Test.check2 "A sphere's radius gets set correctly"
            Random.point3d
            Random.length
            (\centerPoint radius ->
                Sphere3d.withRadius radius centerPoint
                    |> Sphere3d.radius
                    |> Expect.quantity (Quantity.abs radius)
            )
        , Test.check2 "A sphere's center points gets set correctly"
            Random.point3d
            Random.length
            (\centerPoint radius ->
                Sphere3d.withRadius radius centerPoint
                    |> Sphere3d.centerPoint
                    |> Expect.point3d centerPoint
            )
        ]


throughPointsManual : Test
throughPointsManual =
    let
        testCases =
            [ ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, 1 ) }
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, -1 ) }
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, 0.5 ) }
              , ( ( 0, 0, -0.75 ), 1.25 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, -0.5 ) }
              , ( ( 0, 0, 0.75 ), 1.25 )
              )
            ]

        inputPoints { c1, c2, c3, c4 } =
            { p1 = Point3d.fromTuple meters c1
            , p2 = Point3d.fromTuple meters c2
            , p3 = Point3d.fromTuple meters c3
            , p4 = Point3d.fromTuple meters c4
            }

        testTitle inputCoordinates expectedSphere =
            "Given " ++ Debug.toString inputCoordinates ++ " throughPoints returns " ++ Debug.toString (sphereFromTuple expectedSphere)
    in
    Test.describe "throughPoints" <|
        List.map
            (\( inputCoordinates, expectedSphere ) ->
                Test.test
                    (testTitle inputCoordinates expectedSphere)
                    (\_ ->
                        let
                            { p1, p2, p3, p4 } =
                                inputPoints inputCoordinates
                        in
                        Sphere3d.throughPoints p1 p2 p3 p4
                            |> Maybe.map (Expect.sphere3d (sphereFromTuple expectedSphere))
                            |> Maybe.withDefault (Expect.fail "throughPoints returned Nothing on valid input")
                    )
            )
            testCases


triangleArea : Point3d Meters coordinates -> Point3d Meters coordinates -> Point3d Meters coordinates -> Area
triangleArea p1 p2 p3 =
    Triangle3d.area (Triangle3d.from p1 p2 p3)


tetrahedronVolume : Point3d Meters coordinates -> Point3d Meters coordinates -> Point3d Meters coordinates -> Point3d Meters coordinates -> Volume
tetrahedronVolume p1 p2 p3 p4 =
    let
        v2 =
            Vector3d.from p1 p2

        v3 =
            Vector3d.from p1 p3

        v4 =
            Vector3d.from p1 p4
    in
    Quantity.abs (v2 |> Vector3d.cross v3 |> Vector3d.dot v4)
        |> Quantity.divideBy 6


validTetrahedron : Point3d Meters coordinates -> Point3d Meters coordinates -> Point3d Meters coordinates -> Point3d Meters coordinates -> Bool
validTetrahedron p1 p2 p3 p4 =
    let
        volume =
            tetrahedronVolume p1 p2 p3 p4

        limit =
            20

        isValid a b c d =
            let
                area =
                    triangleArea a b c

                isValidTriangle p q r =
                    let
                        base =
                            Point3d.distanceFrom p q

                        hIdeal =
                            base |> Quantity.multiplyBy (sqrt 3 / 2)

                        hMeasured =
                            Quantity.multiplyBy 2 (area |> Quantity.over base)

                        hRatio =
                            Quantity.ratio hIdeal hMeasured
                    in
                    (1 / limit) < hRatio && hRatio < limit
            in
            isValidTriangle a b c
                && isValidTriangle b c a
                && isValidTriangle c a b
                && (let
                        yIdeal =
                            Quantity.sqrt
                                (Quantity.multiplyBy 8 area
                                    |> Quantity.divideBy (3 * sqrt 3)
                                )

                        yMeasured =
                            Quantity.multiplyBy 3 (volume |> Quantity.over area)

                        ratio =
                            Quantity.ratio yIdeal yMeasured
                    in
                    (1 / limit) < ratio && ratio < limit
                   )
    in
    isValid p1 p2 p3 p4
        && isValid p2 p3 p4 p1
        && isValid p3 p4 p1 p2
        && isValid p4 p1 p2 p3


type FourPoints coordinates
    = FourPoints (Point3d Meters coordinates) (Point3d Meters coordinates) (Point3d Meters coordinates) (Point3d Meters coordinates)


throughPointsCheck : Test
throughPointsCheck =
    Test.check4 "All given points lie on the sphere constructed using `throughPoints`"
        Random.point3d
        Random.point3d
        Random.point3d
        Random.point3d
        (\p1 p2 p3 p4 ->
            if validTetrahedron p1 p2 p3 p4 then
                let
                    maybeSphere =
                        Sphere3d.throughPoints p1 p2 p3 p4

                    hasPointOnSurface point sphere =
                        Point3d.distanceFrom point (Sphere3d.centerPoint sphere)
                            |> Expect.quantity (Sphere3d.radius sphere)
                in
                case maybeSphere of
                    Just sphere ->
                        sphere
                            |> Expect.all
                                [ hasPointOnSurface p1
                                , hasPointOnSurface p2
                                , hasPointOnSurface p3
                                , hasPointOnSurface p4
                                ]

                    Nothing ->
                        Expect.fail "throughPoints returned Nothing on valid input"

            else
                Expect.pass
        )


properties : Test
properties =
    Test.check "A sphere has the correct properties (diameter, circumference, surfaceArea, volume)"
        Random.sphere3d
        (\sphere ->
            let
                r =
                    Sphere3d.radius sphere
            in
            sphere
                |> Expect.all
                    [ Sphere3d.diameter
                        >> Expect.quantity
                            (Quantity.multiplyBy 2 r)
                    , Sphere3d.circumference
                        >> Expect.quantity
                            (Quantity.multiplyBy (2 * pi) r)
                    , Sphere3d.surfaceArea
                        >> Expect.quantity
                            (Quantity.multiplyBy (4 * pi) (Quantity.squared r))
                    , Sphere3d.volume
                        >> Expect.quantity
                            (Quantity.multiplyBy (4 / 3 * pi) (Quantity.cubed r))
                    ]
        )


scaleAbout : Test
scaleAbout =
    Test.describe "scaleAbout"
        [ Test.test
            "scaleAbout correctly scales a specific sphere"
            (\_ ->
                let
                    scale =
                        0.5

                    aboutPoint =
                        Point3d.fromTuple meters ( 1, 0, 0 )

                    originalCenter =
                        Point3d.fromTuple meters ( 1, 2, 0 )

                    scaledCenter =
                        Point3d.fromTuple meters ( 1, 1, 0 )

                    originalRadius =
                        meters 2

                    scaledRadius =
                        meters 1
                in
                Sphere3d.withRadius originalRadius originalCenter
                    |> Sphere3d.scaleAbout aboutPoint scale
                    |> Expect.sphere3d (Sphere3d.withRadius scaledRadius scaledCenter)
            )
        , Test.check2 "scaling by 1 has no effect on the sphere"
            Random.point3d
            Random.sphere3d
            (\point sphere ->
                sphere
                    |> Sphere3d.scaleAbout point 1
                    |> Expect.sphere3d sphere
            )
        , Test.check3 "scaling and unscaling has no effect on the sphere"
            Random.point3d
            (Random.int 2 10)
            Random.sphere3d
            (\point scale sphere ->
                sphere
                    |> Sphere3d.scaleAbout point (toFloat scale)
                    |> Sphere3d.scaleAbout point (1 / toFloat scale)
                    |> Expect.sphere3d sphere
            )
        , Test.check3 "scaling changes the radius and distance to the scaling point"
            Random.point3d
            Random.scale
            Random.sphere3d
            (\point scale sphere ->
                let
                    originalRadius =
                        Sphere3d.radius sphere

                    originalDistance =
                        Sphere3d.centerPoint sphere
                            |> Point3d.distanceFrom point
                in
                sphere
                    |> Sphere3d.scaleAbout point scale
                    |> Expect.all
                        [ Sphere3d.radius
                            >> Expect.quantity
                                (originalRadius |> Quantity.multiplyBy (abs scale))
                        , Sphere3d.centerPoint
                            >> Point3d.distanceFrom point
                            >> Expect.quantity
                                (originalDistance |> Quantity.multiplyBy (abs scale))
                        ]
            )
        ]


rotateAround : Test
rotateAround =
    Test.describe "rotateAround"
        [ Test.test
            "correctly rotates a specific sphere"
            (\_ ->
                let
                    axis =
                        Axis3d.z

                    angle =
                        Angle.degrees 90

                    originalCenter =
                        Point3d.fromTuple meters ( 2, 0, 0 )

                    rotatedCenter =
                        Point3d.fromTuple meters ( 0, 2, 0 )

                    radius =
                        meters 2
                in
                Sphere3d.withRadius radius originalCenter
                    |> Sphere3d.rotateAround axis angle
                    |> Expect.sphere3d (Sphere3d.withRadius radius rotatedCenter)
            )
        , Test.check3 "roating a sphere doesn't change its radius and its distance from the rotation axis"
            Random.axis3d
            Random.angle
            Random.sphere3d
            (\axis angle sphere ->
                let
                    originalDistance =
                        sphere
                            |> Sphere3d.centerPoint
                            |> Point3d.distanceFromAxis axis
                in
                sphere
                    |> Sphere3d.rotateAround axis angle
                    |> Expect.all
                        [ Sphere3d.radius
                            >> Expect.quantity (Sphere3d.radius sphere)
                        , Sphere3d.centerPoint
                            >> Point3d.distanceFromAxis axis
                            >> Expect.quantity originalDistance
                        ]
            )
        , Test.check3 "rotates by the correct angle"
            Random.axis3d
            (Random.map Angle.radians (Random.float 0 pi))
            Random.sphere3d
            (\axis angle sphere ->
                let
                    centerDistanceFromAxis =
                        Point3d.distanceFromAxis axis
                            (Sphere3d.centerPoint sphere)
                in
                if
                    centerDistanceFromAxis
                        |> Quantity.greaterThan (meters 0.001)
                then
                    let
                        orthogonalDirectionFromAxisTo point =
                            point
                                |> Point3d.projectOntoAxis axis
                                |> Direction3d.from point

                        maybeOriginalDirection =
                            orthogonalDirectionFromAxisTo
                                (Sphere3d.centerPoint sphere)

                        maybeRotatedDirection =
                            sphere
                                |> Sphere3d.rotateAround axis angle
                                |> Sphere3d.centerPoint
                                |> orthogonalDirectionFromAxisTo
                    in
                    case ( maybeOriginalDirection, maybeRotatedDirection ) of
                        ( Just originalDirection, Just rotatedDirection ) ->
                            let
                                angleBetweenDirections =
                                    Direction3d.angleFrom originalDirection
                                        rotatedDirection
                            in
                            angleBetweenDirections
                                |> Expect.quantity angle

                        _ ->
                            Expect.fail "Failed to compute radial directions"

                else
                    -- Don't bother checking with very small radii since
                    -- computing directions and angles is then prone to roundoff
                    Expect.pass
            )
        , Test.check2 "rotating by 2 pi doesn't change the sphere"
            Random.axis3d
            Random.sphere3d
            (\axis sphere ->
                sphere
                    |> Sphere3d.rotateAround axis (Angle.radians (2 * pi))
                    |> Expect.sphere3d sphere
            )
        , Test.check3 "rotating and unrotating doesn't change the sphere"
            Random.axis3d
            Random.angle
            Random.sphere3d
            (\axis angle sphere ->
                sphere
                    |> Sphere3d.rotateAround axis angle
                    |> Sphere3d.rotateAround axis (Quantity.negate angle)
                    |> Expect.sphere3d sphere
            )
        ]


translateBy : Test
translateBy =
    Test.describe "translateBy"
        [ Test.check2 "translating a sphere doesn't change its radius"
            Random.vector3d
            Random.sphere3d
            (\vector sphere ->
                sphere
                    |> Sphere3d.translateBy vector
                    |> Sphere3d.radius
                    |> Expect.quantity (Sphere3d.radius sphere)
            )
        , Test.check2 "a sphere is translated by the right amount"
            Random.vector3d
            Random.sphere3d
            (\vector sphere ->
                sphere
                    |> Sphere3d.translateBy vector
                    |> Sphere3d.centerPoint
                    |> Vector3d.from (Sphere3d.centerPoint sphere)
                    |> Expect.vector3d vector
            )
        , Test.check "translating by the zero vector doesn't change the sphere"
            Random.sphere3d
            (\sphere ->
                sphere
                    |> Sphere3d.translateBy Vector3d.zero
                    |> Expect.sphere3d sphere
            )
        ]


mirrorAcross : Test
mirrorAcross =
    Test.describe "mirrorAcross"
        [ Test.test
            "mirrors a specific sphere correctly"
            (\_ ->
                let
                    plane =
                        Plane3d.xy

                    originalCenter =
                        Point3d.fromTuple meters ( 2, 2, 1 )

                    mirroredCenter =
                        Point3d.fromTuple meters ( 2, 2, -1 )

                    radius =
                        meters 0.5
                in
                Sphere3d.withRadius radius originalCenter
                    |> Sphere3d.mirrorAcross plane
                    |> Expect.sphere3d
                        (Sphere3d.withRadius radius mirroredCenter)
            )
        , Test.check2 "mirroring a sphere doesn't change its radius"
            Random.plane3d
            Random.sphere3d
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.radius
                    |> Expect.quantity (Sphere3d.radius sphere)
            )
        , Test.check2 "mirroring a sphere across a plane doesn't change the distance from the plane"
            Random.plane3d
            Random.sphere3d
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.centerPoint
                    |> Point3d.signedDistanceFrom plane
                    |> Expect.quantity
                        (Quantity.negate <|
                            Point3d.signedDistanceFrom plane <|
                                Sphere3d.centerPoint sphere
                        )
            )
        , Test.check2 "mirroring a sphere twice yields the original sphere"
            Random.plane3d
            Random.sphere3d
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.mirrorAcross plane
                    |> Expect.sphere3d sphere
            )
        ]


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.check "A sphere's bounding box contains its center point"
        Random.sphere3d
        (\sphere ->
            let
                boundingBox =
                    Sphere3d.boundingBox sphere

                centerPoint =
                    Sphere3d.centerPoint sphere
            in
            if BoundingBox3d.contains centerPoint boundingBox then
                Expect.pass

            else
                Expect.fail "Sphere bounding box does not contain the center point"
        )
