module Tests.BoundingBox2d exposing
    ( boxContainsOwnCenterPoint
    , hullNConsistentWithHull2
    , hullNIsOrderIndependent
    , intersectionConsistentWithIntersects
    , intersectionConsistentWithOverlappingBy
    , intersectionIsValidOrNothing
    , offsetByHalfHeightIsValidOrNothing
    , offsetByHalfWidthIsValidOrNothing
    , offsetResultIsValidOrNothing
    , overlappingBoxesCannotBySeparated
    , overlappingByDetectsIntersection
    , separatedBoxesCannotBeMadeToOverlap
    , separationIsCorrectForDiagonallyDisplacedBoxes
    , separationIsCorrectForHorizontallyDisplacedBoxes
    , separationIsCorrectForVerticallyDisplacedBoxes
    , unionContainsInputs
    )

import BoundingBox2d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Quantity
import Random
import Test exposing (Test)
import Test.Check as Test
import Vector2d


intersectionConsistentWithIntersects : Test
intersectionConsistentWithIntersects =
    Test.check2 "'intersection' is consistent with 'intersects'"
        Random.boundingBox2d
        Random.boundingBox2d
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
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " considered to intersect, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just intersectionBox ) ->
                    Expect.fail
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " not considered to intersect, "
                            ++ " but have valid intersection "
                            ++ Debug.toString intersectionBox
                        )
        )


intersectionConsistentWithOverlappingBy : Test
intersectionConsistentWithOverlappingBy =
    Test.check2 "'intersection' is consistent with 'overlappingByAtLeast'"
        Random.boundingBox2d
        Random.boundingBox2d
        (\first second ->
            let
                overlapping =
                    BoundingBox2d.overlappingByAtLeast Quantity.zero
                        first
                        second

                intersection =
                    BoundingBox2d.intersection first second
            in
            overlapping
                |> Expect.equal
                    (intersection /= Nothing)
        )


unionContainsInputs : Test
unionContainsInputs =
    Test.check2 "union of two boxes contains both input boxes"
        Random.boundingBox2d
        Random.boundingBox2d
        (\first second ->
            let
                union =
                    BoundingBox2d.union first second

                isContained =
                    BoundingBox2d.isContainedIn union
            in
            if isContained first && isContained second then
                Expect.pass

            else
                Expect.fail "Bounding box union does not contain both inputs"
        )


intersectionIsValidOrNothing : Test
intersectionIsValidOrNothing =
    Test.check2 "intersection of two boxes is either Nothing or Just a valid box"
        Random.boundingBox2d
        Random.boundingBox2d
        (\first second ->
            case BoundingBox2d.intersection first second of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox2d result
        )


boxContainsOwnCenterPoint : Test
boxContainsOwnCenterPoint =
    Test.check1 "a bounding box contains its own center point"
        Random.boundingBox2d
        (\box ->
            let
                centerPoint =
                    BoundingBox2d.centerPoint box
            in
            if BoundingBox2d.contains centerPoint box then
                Expect.pass

            else
                Expect.fail "bounding box does not contain its own center point"
        )


overlappingByDetectsIntersection : Test
overlappingByDetectsIntersection =
    Test.check2 "overlappingByAtLeast detects non-intersecting boxes"
        Random.boundingBox2d
        Random.boundingBox2d
        (\firstBox secondBox ->
            case BoundingBox2d.intersection firstBox secondBox of
                Just intersectionBox ->
                    if BoundingBox2d.overlappingByAtLeast Quantity.zero firstBox secondBox then
                        Expect.pass

                    else
                        Expect.fail "intersecting boxes should overlap by at least 0"

                Nothing ->
                    if BoundingBox2d.overlappingByAtLeast Quantity.zero firstBox secondBox then
                        Expect.fail "non-intersecting boxes should overlap by less than 0"

                    else
                        Expect.pass
        )


overlappingBoxesCannotBySeparated : Test
overlappingBoxesCannotBySeparated =
    Test.check3 "boxes overlapping by greater than a distance cannot be separated by moving that distance"
        Random.boundingBox2d
        Random.boundingBox2d
        Random.vector2d
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.overlappingByAtLeast tolerance firstBox secondBox then
                if
                    BoundingBox2d.translateBy displacement firstBox
                        |> BoundingBox2d.intersects secondBox
                then
                    Expect.pass

                else
                    Expect.fail "displaced box should still intersect the other box"

            else
                Expect.pass
        )


separatedBoxesCannotBeMadeToOverlap : Test
separatedBoxesCannotBeMadeToOverlap =
    Test.check3 "boxes separated by greater than a distance cannot be made to overlap by moving that distance"
        Random.boundingBox2d
        Random.boundingBox2d
        Random.vector2d
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.separatedByAtLeast tolerance firstBox secondBox then
                if
                    BoundingBox2d.translateBy displacement firstBox
                        |> BoundingBox2d.intersects secondBox
                then
                    Expect.fail "displaced box should still not intersect the other box"

                else
                    Expect.pass

            else
                Expect.pass
        )


separationIsCorrectForHorizontallyDisplacedBoxes : Test
separationIsCorrectForHorizontallyDisplacedBoxes =
    let
        firstBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 0
                , minY = Quantity.float 0
                , maxX = Quantity.float 1
                , maxY = Quantity.float 1
                }

        secondBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 2
                , minY = Quantity.float 0
                , maxX = Quantity.float 3
                , maxY = Quantity.float 1
                }
    in
    Test.describe "separation is determined correctly for horizontally displaced boxes"
        [ Test.test "separation by 0.5" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0.5) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0.5"
        , Test.test "separation by 0" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0"
        , Test.test "separation by -1" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float -1) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than -1"
        , Test.test "separation by 0.99" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0.99) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0.99"
        , Test.test "separation by 1.01" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 1.01) secondBox firstBox then
                    Expect.fail "Expected separation to not be greater than 1.01"

                else
                    Expect.pass
        ]


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    let
        firstBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 0
                , minY = Quantity.float 0
                , maxX = Quantity.float 1
                , maxY = Quantity.float 1
                }

        secondBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 0
                , minY = Quantity.float 2
                , maxX = Quantity.float 1
                , maxY = Quantity.float 3
                }
    in
    Test.describe "separation is determined correctly for vertically displaced boxes"
        [ Test.test "separation by 0.5" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0.5) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0.5"
        , Test.test "separation by 0" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0"
        , Test.test "separation by -1" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float -1) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than -1"
        , Test.test "separation by 0.99" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0.99) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0.99"
        , Test.test "separation by 1.01" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 1.01) secondBox firstBox then
                    Expect.fail "Expected separation to not be greater than 1.01"

                else
                    Expect.pass
        ]


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    let
        firstBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 0
                , minY = Quantity.float 0
                , maxX = Quantity.float 1
                , maxY = Quantity.float 1
                }

        secondBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float 4
                , minY = Quantity.float 5
                , maxX = Quantity.float 5
                , maxY = Quantity.float 6
                }
    in
    Test.describe "separation is determined correctly for diagonally displaced boxes"
        [ Test.test "separation by 4" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 4) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 4"
        , Test.test "separation by 0" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 0) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 0"
        , Test.test "separation by -1" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float -1) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than -1"
        , Test.test "separation by 4.99" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 4.99) secondBox firstBox then
                    Expect.pass

                else
                    Expect.fail "Expected separation to be greater than 4.99"
        , Test.test "separation by 5.01" <|
            \() ->
                if BoundingBox2d.separatedByAtLeast (Quantity.float 5.01) secondBox firstBox then
                    Expect.fail "Expected separation to not be greater than 5.01"

                else
                    Expect.pass
        ]


offsetResultIsValidOrNothing : Test
offsetResultIsValidOrNothing =
    Test.check2 "offsetBy returns either Nothing or Just a valid box"
        Random.boundingBox2d
        Random.length
        (\boundingBox offset ->
            case BoundingBox2d.offsetBy offset boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox2d result
        )


offsetByHalfWidthIsValidOrNothing : Test
offsetByHalfWidthIsValidOrNothing =
    Test.check1 "offsetBy returns either Nothing or Just a valid box when offseting by -width / 2"
        Random.boundingBox2d
        (\boundingBox ->
            let
                ( width, height ) =
                    BoundingBox2d.dimensions boundingBox

                negativeHalfWidth =
                    Quantity.multiplyBy -0.5 width
            in
            case BoundingBox2d.offsetBy negativeHalfWidth boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox2d result
        )


offsetByHalfHeightIsValidOrNothing : Test
offsetByHalfHeightIsValidOrNothing =
    Test.check1 "offsetBy returns either Nothing or Just a valid box when offseting by -height / 2"
        Random.boundingBox2d
        (\boundingBox ->
            let
                ( width, height ) =
                    BoundingBox2d.dimensions boundingBox

                negativeHalfHeight =
                    Quantity.multiplyBy -0.5 height
            in
            case BoundingBox2d.offsetBy negativeHalfHeight boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox2d result
        )


hullNConsistentWithHull2 : Test
hullNConsistentWithHull2 =
    Test.check2 "'hullN' is consistent with 'from'"
        Random.point2d
        Random.point2d
        (\firstPoint secondPoint ->
            BoundingBox2d.hullN [ firstPoint, secondPoint ]
                |> Expect.equal
                    (Just (BoundingBox2d.from firstPoint secondPoint))
        )


hullNIsOrderIndependent : Test
hullNIsOrderIndependent =
    Test.check1 "'hullN' does not depend on input order"
        (Random.smallList Random.point2d)
        (\points ->
            BoundingBox2d.hullN (List.reverse points)
                |> Expect.equal (BoundingBox2d.hullN points)
        )
