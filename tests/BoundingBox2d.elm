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
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox2d
        Encode.boundingBox2d
        Decode.boundingBox2d


fromPointsConsistentWithContaining : Test
fromPointsConsistentWithContaining =
    Test.fuzz3 Fuzz.point2d
        Fuzz.point2d
        Fuzz.point2d
        "'containing' is consistent with 'containing3'"
        (\first second third ->
            let
                list =
                    [ first, second, third ]

                tuple =
                    ( first, second, third )
            in
                BoundingBox2d.containing list
                    |> Expect.equal (Just (BoundingBox2d.containing3 tuple))
        )


intersectionConsistentWithOverlaps : Test
intersectionConsistentWithOverlaps =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'overlaps'"
        (\first second ->
            let
                overlaps =
                    BoundingBox2d.overlaps first second

                intersection =
                    BoundingBox2d.intersection first second
            in
                case ( overlaps, intersection ) of
                    ( True, Just _ ) ->
                        Expect.pass

                    ( False, Nothing ) ->
                        Expect.pass

                    ( True, Nothing ) ->
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " considered to overlap, "
                                ++ "but intersection is Nothing"
                            )

                    ( False, Just intersectionBox ) ->
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " not considered to overlap, "
                                ++ " but have valid intersection "
                                ++ toString intersectionBox
                            )
        )


hullContainsInputs : Test
hullContainsInputs =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "hull of two boxes contains both input boxes"
        (\first second ->
            let
                hull =
                    BoundingBox2d.hull first second

                containsFirst =
                    BoundingBox2d.contains first hull

                containsSecond =
                    BoundingBox2d.contains second hull
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
