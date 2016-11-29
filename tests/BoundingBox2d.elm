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


module BoundingBox2d exposing (suite)

import Json.Decode as Decode
import Test exposing (Test)
import Fuzz
import Expect
import Test.Runner.Html as HtmlRunner
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Geometry.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox2d
        Encode.boundingBox2d
        Decode.boundingBox2d


containingConsistentWithHull : Test
containingConsistentWithHull =
    Test.fuzz2 Fuzz.point2d
        Fuzz.point2d
        "'containing' is consistent with 'Point2d.hull'"
        (\firstPoint secondPoint ->
            BoundingBox2d.containing [ firstPoint, secondPoint ]
                |> Expect.equal (Just (Point2d.hull firstPoint secondPoint))
        )


containingIsOrderIndependent : Test
containingIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "'containing' does not depend on input order"
        (\points ->
            BoundingBox2d.containing (List.reverse points)
                |> Expect.equal (BoundingBox2d.containing points)
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

                isContained =
                    BoundingBox2d.isContainedIn hull
            in
                Expect.true "Bounding box hull does not contain both inputs"
                    (isContained first && isContained second)
        )


intersectionIsValidOrNothing : Test
intersectionIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "intersection of two boxes is either Nothing or Just a valid box"
        (\first second ->
            case (BoundingBox2d.intersection first second) of
                Nothing ->
                    Expect.pass

                Just result ->
                    let
                        { minX, maxX, minY, maxY } =
                            BoundingBox2d.extrema result
                    in
                        Expect.true "expected extrema to be correctly ordered"
                            ((minX <= maxX) && (minY <= maxY))
        )


boxContainsOwnCentroid : Test
boxContainsOwnCentroid =
    Test.fuzz Fuzz.boundingBox2d
        "a bounding box contains its own centroid"
        (\box ->
            let
                centroid =
                    BoundingBox2d.centroid box
            in
                Expect.true "bounding box does not contain its own centroid"
                    (BoundingBox2d.contains centroid box)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Geometry.BoundingBox2d"
        [ jsonRoundTrips
        , containingConsistentWithHull
        , containingIsOrderIndependent
        , intersectionConsistentWithOverlaps
        , hullContainsInputs
        , intersectionIsValidOrNothing
        , boxContainsOwnCentroid
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
