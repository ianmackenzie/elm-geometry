{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module BoundingBox3d exposing (suite)

import Json.Decode as Decode
import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Fuzz as Fuzz
import OpenSolid.Core.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox3d
        Encode.boundingBox3d
        Decode.boundingBox3d


fromPointsConsistentWithContaining : Test
fromPointsConsistentWithContaining =
    Test.fuzz3 Fuzz.point3d
        Fuzz.point3d
        Fuzz.point3d
        "'containing' is consistent with 'containing3'"
        (\first second third ->
            let
                list =
                    [ first, second, third ]

                tuple =
                    ( first, second, third )
            in
                BoundingBox3d.containing list
                    |> Expect.equal (Just (BoundingBox3d.containing3 tuple))
        )


intersectionConsistentWithOverlaps : Test
intersectionConsistentWithOverlaps =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "'intersection' is consistent with 'overlaps'"
        (\first second ->
            let
                overlaps =
                    BoundingBox3d.overlaps first second

                intersection =
                    BoundingBox3d.intersection first second
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
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "hull of two boxes contains both input boxes"
        (\first second ->
            let
                hull =
                    BoundingBox3d.hull first second

                isContained =
                    BoundingBox3d.isContainedIn hull
            in
                Expect.true "Bounding box hull does not contain both inputs"
                    (isContained first && isContained second)
        )


intersectionIsValidOrNothing : Test
intersectionIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "intersection of two boxes is either Nothing or Just a valid box"
        (\first second ->
            case (BoundingBox3d.intersection first second) of
                Nothing ->
                    Expect.pass

                Just result ->
                    let
                        { minX, maxX, minY, maxY, minZ, maxZ } =
                            BoundingBox3d.extrema result
                    in
                        Expect.true "expected extrema to be correctly ordered"
                            ((minX <= maxX) && (minY <= maxY) && (minZ <= maxZ))
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.BoundingBox3d"
        [ jsonRoundTrips
        , fromPointsConsistentWithContaining
        , intersectionConsistentWithOverlaps
        , hullContainsInputs
        , intersectionIsValidOrNothing
        ]


main : Program Never
main =
    Html.run suite
