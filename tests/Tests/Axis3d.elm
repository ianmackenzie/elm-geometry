module Tests.Axis3d exposing
    ( directionExample
    , intersectionWithPlane
    , onExamples
    , originPointExample
    , throughPoints
    , xExample
    , yExample
    , zExample
    )

import Angle
import Axis2d
import Axis3d
import Direction2d
import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import Plane3d
import Point2d
import Point3d
import Quantity
import SketchPlane3d
import Test exposing (Test)


xExample : Test
xExample =
    Test.test "Axis3d.x example" <|
        \() ->
            Axis3d.x
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.x)


yExample : Test
yExample =
    Test.test "Axis3d.y example" <|
        \() ->
            Axis3d.y
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.y)


zExample : Test
zExample =
    Test.test "Axis3d.z example" <|
        \() ->
            Axis3d.z
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.z)


originPointExample : Test
originPointExample =
    Test.test "Axis3d.originPoint example" <|
        \() ->
            Axis3d.originPoint Axis3d.x |> Expect.point3d Point3d.origin


directionExample : Test
directionExample =
    Test.test "Axis3d.direction example" <|
        \() ->
            Axis3d.direction Axis3d.y |> Expect.direction3d Direction3d.y


onExamples : Test
onExamples =
    let
        axis2d =
            Axis2d.through
                (Point2d.fromTuple meters ( 1, 3 ))
                (Direction2d.fromAngle (Angle.degrees 30))
    in
    Test.describe "Axis3d.on examples"
        [ Test.test "First example" <|
            \() ->
                Axis3d.on SketchPlane3d.xy axis2d
                    |> Expect.axis3d
                        (Axis3d.through
                            (Point3d.fromTuple meters ( 1, 3, 0 ))
                            (Direction3d.fromAzimuthInAndElevationFrom
                                SketchPlane3d.xy
                                (Angle.degrees 30)
                                (Angle.degrees 0)
                            )
                        )
        , Test.test "Second example" <|
            \() ->
                Axis3d.on SketchPlane3d.zx axis2d
                    |> Expect.axis3d
                        (Axis3d.through
                            (Point3d.fromTuple meters ( 3, 0, 1 ))
                            (Direction3d.fromAzimuthInAndElevationFrom
                                SketchPlane3d.xy
                                (Angle.degrees 0)
                                (Angle.degrees 60)
                            )
                        )
        ]


intersectionWithPlane : Test
intersectionWithPlane =
    Test.fuzz2
        Fuzz.axis3d
        Fuzz.plane3d
        "intersectionWithPlane works properly"
        (\axis plane ->
            case Axis3d.intersectionWithPlane plane axis of
                Just point ->
                    point
                        |> Expect.all
                            [ Point3d.signedDistanceFrom plane >> Expect.approximately Quantity.zero
                            , Point3d.distanceFromAxis axis >> Expect.approximately Quantity.zero
                            ]

                Nothing ->
                    Axis3d.direction axis
                        |> Direction3d.componentIn (Plane3d.normalDirection plane)
                        |> Expect.float 0
        )


throughPoints : Test
throughPoints =
    Test.fuzz2
        Fuzz.point3d
        Fuzz.point3d
        "throughPoints"
        (\firstPoint secondPoint ->
            case Axis3d.through firstPoint secondPoint of
                Just axis ->
                    Expect.all
                        [ \axis ->
                            Axis3d.originPoint axis |> Expect.point3d firstPoint
                        , \axis ->
                            Point3d.distanceFrom axis secondPoint
                                |> Expect.approximately Quantity.zero
                        ]
                        axis

                Nothing ->
                    firstPoint |> Expect.point3d secondPoint
        )
