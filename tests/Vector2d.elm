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


module Vector2d exposing (suite)

import Test exposing (Test)
import Fuzz
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector2d Encode.vector2d Decode.vector2d


perpendicularVectorIsPerpendicular : Test
perpendicularVectorIsPerpendicular =
    Test.fuzz Fuzz.vector2d
        "perpendicularTo actually returns a perpendicular vector"
        (\vector ->
            vector
                |> Vector2d.perpendicularTo
                |> Vector2d.dotProduct vector
                |> Expect.approximately 0
        )


dotProductWithSelfIsSquaredLength : Test
dotProductWithSelfIsSquaredLength =
    Test.fuzz Fuzz.vector2d
        "Dot product of a vector with itself is its squared length"
        (\vector ->
            Vector2d.dotProduct vector vector
                |> Expect.approximately (Vector2d.squaredLength vector)
        )


rotateByPreservesLength : Test
rotateByPreservesLength =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.scalar
        "Rotating a vector preserves its length"
        (\vector angle ->
            Vector2d.rotateBy angle vector
                |> Vector2d.length
                |> Expect.approximately (Vector2d.length vector)
        )


rotateByRotatesByTheCorrectAngle : Test
rotateByRotatesByTheCorrectAngle =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.scalar
        "Rotating a vector rotates by the correct angle"
        (\vector angle ->
            let
                direction =
                    Vector2d.direction vector

                rotatedDirection =
                    Vector2d.direction (Vector2d.rotateBy angle vector)

                measuredAngle =
                    Maybe.map2 Direction2d.angleFrom direction rotatedDirection
                        |> Maybe.withDefault 0
            in
                Expect.angle angle measuredAngle
        )


mirrorAcrossPreservesParallelComponent : Test
mirrorAcrossPreservesParallelComponent =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.axis2d
        "Mirroring a vector across an axis preserves component parallel to the axis"
        (\vector axis ->
            let
                parallelComponent =
                    Vector2d.componentIn (Axis2d.direction axis)
            in
                vector
                    |> Vector2d.mirrorAcross axis
                    |> parallelComponent
                    |> Expect.approximately (parallelComponent vector)
        )


mirrorAcrossNegatesPerpendicularComponent : Test
mirrorAcrossNegatesPerpendicularComponent =
    Test.fuzz2 Fuzz.vector2d
        Fuzz.axis2d
        "Mirroring a vector across an axis negates component perpendicular to the axis"
        (\vector axis ->
            let
                perpendicularDirection =
                    Direction2d.perpendicularTo (Axis2d.direction axis)

                perpendicularComponent =
                    Vector2d.componentIn perpendicularDirection
            in
                vector
                    |> Vector2d.mirrorAcross axis
                    |> perpendicularComponent
                    |> Expect.approximately (-(perpendicularComponent vector))
        )


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple ( Fuzz.vector2d, Fuzz.vector2d ))
        "orthonormalize produces a valid frame basis"
        (\vectors ->
            case Vector2d.orthonormalize vectors of
                Just ( xDirection, yDirection ) ->
                    Expect.validFrame2d
                        (Frame2d
                            { originPoint = Point2d.origin
                            , xDirection = xDirection
                            , yDirection = yDirection
                            }
                        )

                Nothing ->
                    let
                        ( v1, v2 ) =
                            vectors

                        crossProduct =
                            Vector2d.crossProduct v1 v2
                    in
                        Expect.approximately 0.0 crossProduct
        )


orthonormalizingParallelVectorsReturnsNothing : Test
orthonormalizingParallelVectorsReturnsNothing =
    Test.test "orthonormalizing parallel vectors returns Nothing"
        (\() ->
            let
                vectors =
                    ( Vector2d ( 1, 2 )
                    , Vector2d ( -3, -6 )
                    )
            in
                Expect.equal Nothing (Vector2d.orthonormalize vectors)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Vector2d"
        [ jsonRoundTrips
        , perpendicularVectorIsPerpendicular
        , dotProductWithSelfIsSquaredLength
        , rotateByPreservesLength
        , rotateByRotatesByTheCorrectAngle
        , mirrorAcrossPreservesParallelComponent
        , mirrorAcrossNegatesPerpendicularComponent
        , orthonormalizeProducesValidFrameBasis
        , orthonormalizingParallelVectorsReturnsNothing
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
