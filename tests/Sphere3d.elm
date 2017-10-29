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
import OpenSolid.Sphere3d as Sphere3d
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, fuzz4, test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips sphere3d
        Encode.sphere3d
        Decode.sphere3d


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


around : Test
around =
    describe "around"
        [ fuzz2 axis3d
            point3d
            "A sphere constructed using `around` does contain the given point"
            (\axis point ->
                let
                    sphere =
                        Sphere3d.around axis point
                in
                Point3d.squaredDistanceFrom point (Sphere3d.centerPoint sphere)
                    |> Expect.atMost (Sphere3d.radius sphere ^ 2 + 1.0e-6)
            )
        , fuzz2 axis3d
            point3d
            "The center point of a sphere constructed using `around` lies on the given axis"
            (\axis point ->
                let
                    sphere =
                        Sphere3d.around axis point
                in
                Sphere3d.centerPoint sphere
                    |> Point3d.squaredDistanceFromAxis axis
                    |> Expect.within (Absolute 1.0e-6) 0
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

        with ( centerPoint, radius ) =
            Sphere3d.with { centerPoint = Point3d.fromCoordinates centerPoint, radius = radius }
    in
    Test.concat <|
        List.map
            (\( inputPoints, expectedSphere ) ->
                test
                    ("Given " ++ toString (input inputPoints) ++ " throughPoints returns " ++ toString (with expectedSphere))
                    (\_ ->
                        Sphere3d.throughPoints (input inputPoints)
                            |> Maybe.map (Expect.equal (with expectedSphere))
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
                -- if the first three points are collinear it is not possible to construct a circle
                -- that passes through them
                notCollinear =
                    Direction3d.from p1 p2
                        |> Maybe.map
                            (\direction ->
                                Axis3d.with { originPoint = p1, direction = direction }
                            )
                        |> Maybe.map
                            (\axis ->
                                Point3d.squaredDistanceFromAxis axis p3 /= 0
                            )

                -- it is not possible to construct a sphere from four points that are all in the same plane
                notInTheSamePlane =
                    Plane3d.throughPoints ( p1, p2, p3 )
                        |> Maybe.map (flip Point3d.signedDistanceFrom p4)
                        |> Maybe.map ((/=) 0)

                isValidInput =
                    Maybe.map2 (&&) notCollinear notInTheSamePlane
                        |> Maybe.withDefault False

                sphere =
                    Sphere3d.throughPoints ( p1, p2, p3, p4 )

                liesOnSphere point sphere =
                    Point3d.squaredDistanceFrom point (Sphere3d.centerPoint sphere)
                        |> Expect.within (Absolute 1.0e-6) (Sphere3d.radius sphere ^ 2)
            in
            case sphere of
                Just sphere ->
                    Expect.all (List.map liesOnSphere [ p1, p2, p3 ]) sphere

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
