module Axis2d
    exposing
        ( directionExample
        , flipExample
        , jsonRoundTrips
        , mirrorAcrossExample
        , moveToExample
        , originPointExample
        , placeInExample
        , relativeToExample
        , rotateAroundExample
        , translateByExample
        , xExample
        , yExample
        )

import Generic
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.axis2d Encode.axis2d Decode.axis2d


xExample : Test
xExample =
    Test.test "Axis2d.x example" <|
        \() ->
            Axis2d.x
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.origin
                        , direction = Direction2d.x
                        }
                    )


yExample : Test
yExample =
    Test.test "Axis2d.y example" <|
        \() ->
            Axis2d.y
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.origin
                        , direction = Direction2d.y
                        }
                    )


originPointExample : Test
originPointExample =
    Test.test "Axis2d.originPoint example" <|
        \() ->
            Axis2d.originPoint Axis2d.x |> Expect.point2d Point2d.origin


directionExample : Test
directionExample =
    Test.test "Axis2d.direction example" <|
        \() ->
            Axis2d.direction Axis2d.y |> Expect.direction2d Direction2d.y


flipExample : Test
flipExample =
    Test.test "Axis2d.flip example" <|
        \() ->
            Axis2d.flip Axis2d.x
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.origin
                        , direction = Direction2d.negativeX
                        }
                    )


moveToExample : Test
moveToExample =
    Test.test "Axis2d.moveTo example" <|
        \() ->
            let
                axis =
                    Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 2, 3 )
                        , direction = Direction2d.y
                        }

                newOrigin =
                    Point2d.fromCoordinates ( 4, 5 )
            in
            Axis2d.moveTo newOrigin axis
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 4, 5 )
                        , direction = Direction2d.y
                        }
                    )


rotateAroundExample : Test
rotateAroundExample =
    Test.test "Axis2d.rotateAround example" <|
        \() ->
            Axis2d.rotateAround Point2d.origin (degrees 90) Axis2d.x
                |> Expect.axis2d Axis2d.y


translateByExample : Test
translateByExample =
    Test.test "Axis2d.translateBy example" <|
        \() ->
            let
                displacement =
                    Vector2d.fromComponents ( 2, 3 )
            in
            Axis2d.translateBy displacement Axis2d.y
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 2, 3 )
                        , direction = Direction2d.y
                        }
                    )


mirrorAcrossExample : Test
mirrorAcrossExample =
    Test.test "Axis2d.mirrorAcross example" <|
        \() ->
            let
                axis =
                    Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 1, 2 )
                        , direction = Direction2d.fromAngle (degrees 30)
                        }
            in
            Axis2d.mirrorAcross Axis2d.x axis
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 1, -2 )
                        , direction = Direction2d.fromAngle (degrees -30)
                        }
                    )


relativeToExample : Test
relativeToExample =
    Test.test "Axis2d.relativeTo example" <|
        \() ->
            let
                originPoint =
                    Point2d.fromCoordinates ( 2, 3 )
            in
            Axis2d.relativeTo (Frame2d.atPoint originPoint) Axis2d.x
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( -2, -3 )
                        , direction = Direction2d.x
                        }
                    )


placeInExample : Test
placeInExample =
    Test.test "Axis2d.placeIn example" <|
        \() ->
            let
                originPoint =
                    Point2d.fromCoordinates ( 2, 3 )
            in
            Axis2d.placeIn (Frame2d.atPoint originPoint) Axis2d.x
                |> Expect.axis2d
                    (Axis2d.with
                        { originPoint = Point2d.fromCoordinates ( 2, 3 )
                        , direction = Direction2d.x
                        }
                    )
