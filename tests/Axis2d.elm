--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Axis2d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as HtmlRunner
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.axis2d Encode.axis2d Decode.axis2d


xExample : Test
xExample =
    Test.test "Axis2d.x example" <|
        \() ->
            Axis2d.x
                |> Expect.axis2d
                    (Axis2d
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
                    (Axis2d
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
                    (Axis2d
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
                    Axis2d
                        { originPoint = Point2d ( 2, 3 )
                        , direction = Direction2d.y
                        }

                newOrigin =
                    Point2d ( 4, 5 )
            in
                Axis2d.moveTo newOrigin axis
                    |> Expect.axis2d
                        (Axis2d
                            { originPoint = Point2d ( 4, 5 )
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
                    Vector2d ( 2, 3 )
            in
                Axis2d.translateBy displacement Axis2d.y
                    |> Expect.axis2d
                        (Axis2d
                            { originPoint = Point2d ( 2, 3 )
                            , direction = Direction2d.y
                            }
                        )


mirrorAcrossExample : Test
mirrorAcrossExample =
    Test.test "Axis2d.mirrorAcross example" <|
        \() ->
            let
                axis =
                    Axis2d
                        { originPoint = Point2d ( 1, 2 )
                        , direction = Direction2d.fromAngle (degrees 30)
                        }
            in
                Axis2d.mirrorAcross Axis2d.x axis
                    |> Expect.axis2d
                        (Axis2d
                            { originPoint = Point2d ( 1, -2 )
                            , direction = Direction2d.fromAngle (degrees -30)
                            }
                        )


relativeToExample : Test
relativeToExample =
    Test.test "Axis2d.relativeTo example" <|
        \() ->
            let
                originPoint =
                    Point2d ( 2, 3 )
            in
                Axis2d.relativeTo (Frame2d.at originPoint) Axis2d.x
                    |> Expect.axis2d
                        (Axis2d
                            { originPoint = Point2d ( -2, -3 )
                            , direction = Direction2d.x
                            }
                        )


placeInExample : Test
placeInExample =
    Test.test "Axis2d.placeIn example" <|
        \() ->
            let
                originPoint =
                    Point2d ( 2, 3 )
            in
                Axis2d.placeIn (Frame2d.at originPoint) Axis2d.x
                    |> Expect.axis2d
                        (Axis2d
                            { originPoint = Point2d ( 2, 3 )
                            , direction = Direction2d.x
                            }
                        )


placeOntoExamples : Test
placeOntoExamples =
    let
        axis =
            Axis2d
                { originPoint = Point2d ( 2, 3 )
                , direction = Direction2d ( 0.6, 0.8 )
                }
    in
        Test.describe "Axis2d.placeOnto examples"
            [ Test.test "First example" <|
                \() ->
                    Axis2d.placeOnto SketchPlane3d.xy axis
                        |> Expect.axis3d
                            (Axis3d
                                { originPoint = Point3d ( 2, 3, 0 )
                                , direction = Direction3d ( 0.6, 0.8, 0 )
                                }
                            )
            , Test.test "Second example" <|
                \() ->
                    Axis2d.placeOnto SketchPlane3d.zx axis
                        |> Expect.axis3d
                            (Axis3d
                                { originPoint = Point3d ( 3, 0, 2 )
                                , direction = Direction3d ( 0.8, 0, 0.6 )
                                }
                            )
            ]


documentationExamples : Test
documentationExamples =
    Test.describe "Documentation examples are correct"
        [ xExample
        , yExample
        , originPointExample
        , directionExample
        , flipExample
        , moveToExample
        , rotateAroundExample
        , translateByExample
        , mirrorAcrossExample
        , relativeToExample
        , placeInExample
        , placeOntoExamples
        ]


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Axis2d"
        [ jsonRoundTrips
        , documentationExamples
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
