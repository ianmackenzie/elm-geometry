{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module BoundingBox2d exposing (suite)

import Json.Decode as Decode
import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Fuzz as Fuzz
import OpenSolid.Core.Expect as Expect


jsonRoundTrips : Test
jsonRoundTrips =
    Test.fuzz Fuzz.boundingBox2d
        "JSON conversion round-trips properly"
        (\value ->
            let
                encoded =
                    Encode.boundingBox2d value

                decoded =
                    Decode.decodeValue Decode.boundingBox2d encoded
            in
                case decoded of
                    Ok result ->
                        if BoundingBox2d.isEmpty value then
                            Expect.true "Expected empty bounding box to round-trip to empty bounding box"
                                (BoundingBox2d.isEmpty result)
                        else
                            Expect.equal result value

                    Err string ->
                        Expect.fail string
        )


fromPointsConsistentWithContaining : Test
fromPointsConsistentWithContaining =
    Test.fuzz2 Fuzz.point2d
        Fuzz.point2d
        "'containing' is consistent with 'fromPoints'"
        (\first second ->
            BoundingBox2d.containing [ first, second ]
                |> Expect.boundingBox2d (BoundingBox2d.fromPoints first second)
        )


intersectionConsistentWithOverlaps : Test
intersectionConsistentWithOverlaps =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'overlaps'"
        (\first second ->
            let
                intersection =
                    BoundingBox2d.intersection first second

                overlaps =
                    BoundingBox2d.overlaps first second
            in
                case ( overlaps, BoundingBox2d.isEmpty intersection ) of
                    ( True, False ) ->
                        Expect.pass

                    ( False, True ) ->
                        Expect.pass

                    ( True, True ) ->
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " considered to overlap, "
                                ++ "but intersection is empty"
                            )

                    ( False, False ) ->
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " not considered to overlap, "
                                ++ " but have non-empty intersection "
                                ++ toString intersection
                            )
        )


hullContainsInputs : Test
hullContainsInputs =
    Test.fuzz2 Fuzz.nonEmptyBoundingBox2d
        Fuzz.nonEmptyBoundingBox2d
        "hull of two non-empty boxes contains both input boxes"
        (\first second ->
            let
                hull =
                    BoundingBox2d.hull first second

                containsFirst =
                    BoundingBox2d.isContainedWithin hull first

                containsSecond =
                    BoundingBox2d.isContainedWithin hull second
            in
                Expect.true "Bounding box hull does not contain both inputs"
                    (containsFirst && containsSecond)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.BoundingBox2d"
        [ jsonRoundTrips
        , fromPointsConsistentWithContaining
        , intersectionConsistentWithOverlaps
        , hullContainsInputs
        ]


main : Program Never
main =
    Html.run suite
