module Axis3d
    exposing
        ( directionExample
        , jsonRoundTrips
        , originPointExample
        , xExample
        , yExample
        , zExample
        )

import Generic
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.axis3d Encode.axis3d Decode.axis3d


xExample : Test
xExample =
    Test.test "Axis3d.x example" <|
        \() ->
            Axis3d.x
                |> Expect.axis3d
                    (Axis3d
                        { originPoint = Point3d.origin
                        , direction = Direction3d.x
                        }
                    )


yExample : Test
yExample =
    Test.test "Axis3d.y example" <|
        \() ->
            Axis3d.y
                |> Expect.axis3d
                    (Axis3d
                        { originPoint = Point3d.origin
                        , direction = Direction3d.y
                        }
                    )


zExample : Test
zExample =
    Test.test "Axis3d.z example" <|
        \() ->
            Axis3d.z
                |> Expect.axis3d
                    (Axis3d
                        { originPoint = Point3d.origin
                        , direction = Direction3d.z
                        }
                    )


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
