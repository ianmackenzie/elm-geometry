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


module Frame2d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as HtmlRunner
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.frame2d Encode.frame2d Decode.frame2d


globalToGlobal : Test
globalToGlobal =
    let
        description =
            "Global -> local -> global conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.relativeTo frame
                |> Point2d.placeIn frame
                |> Expect.point2d point
    in
        Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation


localToLocal : Test
localToLocal =
    let
        description =
            "Local -> global -> local conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.placeIn frame
                |> Point2d.relativeTo frame
                |> Expect.point2d point
    in
        Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.Frame2d"
        [ jsonRoundTrips
        , globalToGlobal
        , localToLocal
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
