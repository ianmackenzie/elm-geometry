module BoundingBox2d
    exposing
        ( boxContainsOwnCentroid
        , hullContainsInputs
        , intersectionConsistentWithOverlaps
        , intersectionIsValidOrNothing
        , jsonRoundTrips
        )

import Expect
import Fuzz
import Generic
import Json.Decode as Decode
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox2d
        Encode.boundingBox2d
        Decode.boundingBox2d


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
            case BoundingBox2d.intersection first second of
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
