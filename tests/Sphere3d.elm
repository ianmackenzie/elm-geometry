module Sphere3d exposing (..)

import Expect exposing (FloatingPointTolerance(Absolute, AbsoluteOrRelative))
import Fuzz
import Generic
import OpenSolid.Arc3d as Arc3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz exposing (arc3d, axis3d, point3d, scalar, sphere3d)
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Sphere3d as Sphere3d exposing (Sphere3d)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.Vector3d as Vector3d
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, fuzz4, test)


{-
   Helper methods
-}


sphereFromTuple : ( ( Float, Float, Float ), Float ) -> Sphere3d
sphereFromTuple ( centerPoint, radius ) =
    Sphere3d.with { centerPoint = Point3d.fromCoordinates centerPoint, radius = radius }



{-
   Tests
-}


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips sphere3d
        Encode.sphere3d
        Decode.sphere3d


unit : Test
unit =
    describe "unit"
        [ test "The sphere returned by unit has radius 1" <|
            \_ ->
                Sphere3d.unit
                    |> Sphere3d.radius
                    |> Expect.equal 1
        , test "The sphere returned by unit is centered on the origin" <|
            \_ ->
                Sphere3d.unit
                    |> Sphere3d.centerPoint
                    |> Expect.equal Point3d.origin
        ]


with : Test
with =
    describe "with"
        [ fuzz2 point3d
            scalar
            "A sphere's radius gets set correctly"
            (\centerPoint radius ->
                let
                    sphere =
                        Sphere3d.with { centerPoint = centerPoint, radius = radius }
                in
                Sphere3d.radius sphere
                    |> Expect.equal (abs radius)
            )
        , fuzz2 point3d
            scalar
            "A sphere's center points gets set correctly"
            (\centerPoint radius ->
                let
                    sphere =
                        Sphere3d.with { centerPoint = centerPoint, radius = radius }
                in
                Sphere3d.centerPoint sphere
                    |> Expect.equal centerPoint
            )
        ]


throughPointsManual : Test
throughPointsManual =
    let
        testCases =
            [ ( ( ( 1, 0, 0 ), ( 0, 1, 0 ), ( -1, 0, 0 ), ( 0, 0, 1 ) )
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( ( ( 1, 0, 0 ), ( 0, 1, 0 ), ( -1, 0, 0 ), ( 0, 0, -1 ) )
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( ( ( 1, 0, 0 ), ( 0, 1, 0 ), ( -1, 0, 0 ), ( 0, 0, 0.5 ) )
              , ( ( 0, 0, -0.75 ), 1.25 )
              )
            , ( ( ( 1, 0, 0 ), ( 0, 1, 0 ), ( -1, 0, 0 ), ( 0, 0, -0.5 ) )
              , ( ( 0, 0, 0.75 ), 1.25 )
              )
            ]

        input ( p1, p2, p3, p4 ) =
            ( Point3d.fromCoordinates p1
            , Point3d.fromCoordinates p2
            , Point3d.fromCoordinates p3
            , Point3d.fromCoordinates p4
            )

        testTitle inputPoints expectedSphere =
            "Given " ++ toString (input inputPoints) ++ " throughPoints returns " ++ toString (sphereFromTuple expectedSphere)
    in
    describe "throughPoints" <|
        List.map
            (\( inputPoints, expectedSphere ) ->
                test
                    (testTitle inputPoints expectedSphere)
                    (\_ ->
                        Sphere3d.throughPoints (input inputPoints)
                            |> Maybe.map (Expect.equal (sphereFromTuple expectedSphere))
                            |> Maybe.withDefault (Expect.fail "throughPoints returned Nothing on valid input")
                    )
            )
            testCases


throughPointsFuzz : Test
throughPointsFuzz =
    fuzz4 point3d
        point3d
        point3d
        point3d
        "All given points lie on the sphere constructed using `throughPoints`"
        (\p1 p2 p3 p4 ->
            let
                {- If the first three of the four points are collinear no circle
                       (and by extension no sphere) can be constructed going through them.
                   If the fourth point is in the same plane as the three other points it is also
                       not possible to construct a sphere going through them (it would need to have a height of zero).
                   ==> volume of the tetrahedron formed by the four points needs to be greater than zero
                -}
                isValidInput =
                    let
                        v2 =
                            Vector3d.from p1 p2

                        v3 =
                            Vector3d.from p1 p3

                        v4 =
                            Vector3d.from p1 p4

                        volume =
                            abs (Vector3d.dotProduct v4 (Vector3d.crossProduct v2 v3))
                    in
                    volume > 1.0e-6

                sphere =
                    Sphere3d.throughPoints ( p1, p2, p3, p4 )

                hasPointOnSurface point sphere =
                    Point3d.distanceFrom point (Sphere3d.centerPoint sphere)
                        |> Expect.within (Absolute 1.0e-6) (Sphere3d.radius sphere)
            in
            case sphere of
                Just sphere ->
                    sphere
                        |> Expect.all
                            [ hasPointOnSurface p1
                            , hasPointOnSurface p2
                            , hasPointOnSurface p3
                            , hasPointOnSurface p4
                            ]

                Nothing ->
                    if isValidInput then
                        Expect.fail "throughPoints returned Nothing on valid input"
                    else
                        Expect.pass
        )


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    fuzz sphere3d
        "A sphere's bounding box contains its center point"
        (\sphere ->
            let
                boundingBox =
                    Sphere3d.boundingBox sphere

                centerPoint =
                    Sphere3d.centerPoint sphere
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox3d.contains centerPoint boundingBox)
        )
