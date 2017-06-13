module BoundingBox3d
    exposing
        ( boxContainsOwnCentroid
        , containingConsistentWithHull
        , containingIsOrderIndependent
        , hullContainsInputs
        , intersectionConsistentWithOverlaps
        , intersectionIsValidOrNothing
        , jsonRoundTrips
        )

import Expect
import Fuzz
import Generic
import Json.Decode as Decode
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point3d as Point3d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox3d
        Encode.boundingBox3d
        Decode.boundingBox3d


containingConsistentWithHull : Test
containingConsistentWithHull =
    Test.fuzz2 Fuzz.point3d
        Fuzz.point3d
        "'containing' is consistent with 'Point3d.hull'"
        (\firstPoint secondPoint ->
            BoundingBox3d.containing [ firstPoint, secondPoint ]
                |> Expect.equal (Just (Point3d.hull firstPoint secondPoint))
        )


containingIsOrderIndependent : Test
containingIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point3d)
        "'containing' does not depend on input order"
        (\points ->
            BoundingBox3d.containing (List.reverse points)
                |> Expect.equal (BoundingBox3d.containing points)
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
            case BoundingBox3d.intersection first second of
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


boxContainsOwnCentroid : Test
boxContainsOwnCentroid =
    Test.fuzz Fuzz.boundingBox3d
        "a bounding box contains its own centroid"
        (\box ->
            let
                centroid =
                    BoundingBox3d.centroid box
            in
            Expect.true "bounding box does not contain its own centroid"
                (BoundingBox3d.contains centroid box)
        )
