module BoundingBox2d
    exposing
        ( boxContainsOwnCentroid
        , hullContainsInputs
        , intersectionConsistentWithIntersects
        , intersectionConsistentWithOverlapsBy
        , intersectionIsValidOrNothing
        , jsonRoundTrips
        , overlappingBoxesCannotBySeparated
        , overlapsByDetectsIntersection
        , separatedBoxesCannotBeMadeToOverlap
        , separationIsCorrectForDiagonallyDisplacedBoxes
        , separationIsCorrectForHorizontallyDisplacedBoxes
        , separationIsCorrectForVerticallyDisplacedBoxes
        )

import Expect
import Generic
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Vector2d as Vector2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.boundingBox2d
        Encode.boundingBox2d
        Decode.boundingBox2d


intersectionConsistentWithIntersects : Test
intersectionConsistentWithIntersects =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'intersects'"
        (\first second ->
            let
                intersects =
                    BoundingBox2d.intersects first second

                intersection =
                    BoundingBox2d.intersection first second
            in
            case ( intersects, intersection ) of
                ( True, Just _ ) ->
                    Expect.pass

                ( False, Nothing ) ->
                    Expect.pass

                ( True, Nothing ) ->
                    Expect.fail
                        (toString first
                            ++ " and "
                            ++ toString second
                            ++ " considered to intersect, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just intersectionBox ) ->
                    Expect.fail
                        (toString first
                            ++ " and "
                            ++ toString second
                            ++ " not considered to intersect, "
                            ++ " but have valid intersection "
                            ++ toString intersectionBox
                        )
        )


intersectionConsistentWithOverlapsBy : Test
intersectionConsistentWithOverlapsBy =
    Test.fuzz2
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'overlapsBy'"
        (\first second ->
            let
                overlaps =
                    BoundingBox2d.overlapsBy GT 0 first second

                intersection =
                    BoundingBox2d.intersection first second

                intersectionDimensions =
                    Maybe.map BoundingBox2d.dimensions intersection
            in
            case ( overlaps, intersectionDimensions ) of
                ( True, Just ( width, height ) ) ->
                    if width == 0 then
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection width is 0"
                            )
                    else if height == 0 then
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection height is 0"
                            )
                    else
                        Expect.pass

                ( False, Nothing ) ->
                    Expect.pass

                ( True, Nothing ) ->
                    Expect.fail
                        (toString first
                            ++ " and "
                            ++ toString second
                            ++ " considered to strictly overlap, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just ( width, height ) ) ->
                    if height == 0 || width == 0 then
                        Expect.pass
                    else
                        Expect.fail
                            (toString first
                                ++ " and "
                                ++ toString second
                                ++ " not considered to strictly overlap, "
                                ++ "but have valid intersection "
                                ++ "with non-zero dimensions "
                                ++ toString intersectionDimensions
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


overlapsByDetectsIntersection : Test
overlapsByDetectsIntersection =
    Test.fuzz2
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "overlapsBy LT 0 detects non-intersecting boxes"
        (\firstBox secondBox ->
            case BoundingBox2d.intersection firstBox secondBox of
                Just intersectionBox ->
                    Expect.false "intersecting boxes should overlap by at least 0"
                        (BoundingBox2d.overlapsBy LT 0 firstBox secondBox)

                Nothing ->
                    Expect.true "non-intersecting boxes should overlap by less than 0"
                        (BoundingBox2d.overlapsBy LT 0 firstBox secondBox)
        )


overlappingBoxesCannotBySeparated : Test
overlappingBoxesCannotBySeparated =
    Test.fuzz3
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        Fuzz.vector2d
        "boxes overlapping by greater than a distance cannot be separated by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.overlapsBy GT tolerance firstBox secondBox then
                BoundingBox2d.translateBy displacement firstBox
                    |> BoundingBox2d.intersects secondBox
                    |> Expect.true "displaced box should still intersect the other box"
            else
                Expect.pass
        )


separatedBoxesCannotBeMadeToOverlap : Test
separatedBoxesCannotBeMadeToOverlap =
    Test.fuzz3
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        Fuzz.vector2d
        "boxes separated by greater than a distance cannot be made to overlap by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.separatedBy GT tolerance firstBox secondBox then
                BoundingBox2d.translateBy displacement firstBox
                    |> BoundingBox2d.intersects secondBox
                    |> Expect.false "displaced box should still not intersect the other box"
            else
                Expect.pass
        )


separationIsCorrectForHorizontallyDisplacedBoxes : Test
separationIsCorrectForHorizontallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for horizontally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox2d.with
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.with
                        { minX = 2
                        , minY = 0
                        , maxX = 3
                        , maxY = 1
                        }
            in
            BoundingBox2d.separatedBy EQ 1 firstBox secondBox
                |> Expect.true "separation is not equal to 1"
        )


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for vertically displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox2d.with
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.with
                        { minX = 0
                        , minY = 2
                        , maxX = 1
                        , maxY = 3
                        }
            in
            BoundingBox2d.separatedBy EQ 1 firstBox secondBox
                |> Expect.true "separation is not equal to 1"
        )


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for diagonally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox2d.with
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.with
                        { minX = 4
                        , minY = 5
                        , maxX = 5
                        , maxY = 6
                        }
            in
            BoundingBox2d.separatedBy EQ 5 firstBox secondBox
                |> Expect.true "separation is not equal to 5"
        )
