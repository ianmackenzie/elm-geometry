module Tests.Axis3d exposing
    ( directionExample
    , intersectionWithPlane
    , intersectionWithSphere
    , intersectionWithTriangle
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
import Fuzz as ElmFuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (meters)
import LineSegment3d
import Plane3d
import Point2d
import Point3d
import Quantity
import SketchPlane3d
import Sphere3d
import Test exposing (Test)
import Triangle3d
import Vector3d


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
            if
                abs
                    (Axis3d.direction axis
                        |> Direction3d.componentIn (Plane3d.normalDirection plane)
                    )
                    > 1.0e-3
            then
                case Axis3d.intersectionWithPlane plane axis of
                    Just point ->
                        point
                            |> Expect.all
                                [ Point3d.signedDistanceFrom plane >> Expect.quantity Quantity.zero
                                , Point3d.distanceFromAxis axis >> Expect.quantity Quantity.zero
                                ]

                    Nothing ->
                        Expect.fail "Expected axis/plane intersection"

            else
                Expect.pass
        )


intersectionWithTriangle : Test
intersectionWithTriangle =
    Test.fuzz3
        Fuzz.triangle3d
        Fuzz.direction3d
        (ElmFuzz.tuple3
            ( ElmFuzz.floatRange -0.5 1.5
            , ElmFuzz.floatRange -0.5 1.5
            , ElmFuzz.floatRange -10 10
            )
        )
        "intersectionWithTriangle works properly"
        (\triangle axisDirection parameters ->
            let
                s =
                    Triangle3d.normalDirection triangle
                        |> Maybe.map (Direction3d.componentIn axisDirection)
                        |> Maybe.withDefault 0
            in
            if abs s < 1.0e-3 then
                Expect.pass

            else
                let
                    ( p0, p1, p2 ) =
                        Triangle3d.vertices triangle

                    e1 =
                        Vector3d.from p0 p1

                    e2 =
                        Vector3d.from p0 p2

                    ( u, v, t ) =
                        parameters

                    planeIntersection =
                        p0
                            |> Point3d.translateBy (Vector3d.scaleBy u e1)
                            |> Point3d.translateBy (Vector3d.scaleBy v e2)

                    direction =
                        Vector3d.withLength (Length.meters 1) axisDirection

                    axisOrigin =
                        planeIntersection
                            |> Point3d.translateBy (Vector3d.scaleBy -t direction)

                    axis =
                        Axis3d.through axisOrigin axisDirection
                in
                case Axis3d.intersectionWithTriangle triangle axis of
                    Nothing ->
                        if u < 0 || v < 0 || u + v > 1 then
                            Expect.pass

                        else
                            Expect.fail "Expected an intersection"

                    Just point ->
                        if u < 0 || v < 0 || u + v > 1 then
                            Expect.fail "Expected no intersection"

                        else
                            Expect.point3d planeIntersection point
        )


intersectionWithSphere : Test
intersectionWithSphere =
    Test.describe "intersectionWithSphere"
        [ Test.test "no intersection points" <|
            \_ ->
                let
                    sphere =
                        Sphere3d.withRadius (meters 1) (Point3d.meters 0 0 0)

                    axis =
                        Axis3d.through (Point3d.meters 6 0 0) Direction3d.y
                in
                Expect.equal (Axis3d.intersectionWithSphere sphere axis) Nothing
        , Test.test "two intersection points" <|
            \_ ->
                let
                    sphere =
                        Sphere3d.withRadius (meters 1) (Point3d.meters 0 0 0)

                    axis =
                        Axis3d.through (Point3d.meters 0 0 0) Direction3d.y
                in
                Expect.equal (Axis3d.intersectionWithSphere sphere axis)
                    (Just ( Point3d.meters 0 -1 0, Point3d.meters 0 1 0 ))
        , Test.test "the same intersection points" <|
            \_ ->
                let
                    sphere =
                        Sphere3d.withRadius (meters 1) (Point3d.meters 0 0 0)

                    axis =
                        Axis3d.through (Point3d.meters 1 0 0) Direction3d.y
                in
                Expect.equal (Axis3d.intersectionWithSphere sphere axis)
                    (Just ( Point3d.meters 1 0 0, Point3d.meters 1 0 0 ))
        , Test.fuzz2
            Fuzz.axis3d
            Fuzz.sphere3d
            "intersection points should be on the sphere and the axis"
            (\axis sphere ->
                case Axis3d.intersectionWithSphere sphere axis of
                    Just pointPair ->
                        let
                            -- An intersection point should be on the sphere
                            -- (have a distance from the sphere center point
                            -- equal to the sphere radius), and on the axis
                            -- (have a zero distance from the axis)
                            validIntersectionPoint point =
                                Expect.all
                                    [ Point3d.distanceFrom (Sphere3d.centerPoint sphere)
                                        >> Expect.quantity (Sphere3d.radius sphere)
                                    , Point3d.distanceFromAxis axis
                                        >> Expect.quantity Quantity.zero
                                    ]
                                    point
                        in
                        -- Both intersection points should be valid
                        Expect.all
                            [ Tuple.first >> validIntersectionPoint
                            , Tuple.second >> validIntersectionPoint
                            ]
                            pointPair

                    Nothing ->
                        -- If the axis does not intersect the sphere, then the
                        -- distance from the sphere center point to the axis
                        -- should be greater than the radius of the sphere (if
                        -- it was less, then there should be an intersection!)
                        Sphere3d.centerPoint sphere
                            |> Point3d.distanceFromAxis axis
                            |> Expect.quantityGreaterThan (Sphere3d.radius sphere)
            )
        ]


throughPoints : Test
throughPoints =
    Test.fuzz2
        Fuzz.point3d
        Fuzz.point3d
        "throughPoints"
        (\firstPoint secondPoint ->
            case Axis3d.throughPoints firstPoint secondPoint of
                Just axis ->
                    Expect.all
                        [ \() ->
                            Axis3d.originPoint axis |> Expect.point3d firstPoint
                        , \() ->
                            Point3d.distanceFromAxis axis secondPoint
                                |> Expect.quantity Quantity.zero
                        ]
                        ()

                Nothing ->
                    firstPoint |> Expect.point3d secondPoint
        )
