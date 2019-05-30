module Tests.BoundingBox3d exposing
    ( boxContainsOwnCenterPoint
    , containingPointsConsistentWithFromCorners
    , containingPointsIsOrderIndependent
    , hullContainsInputs
    , intersectionConsistentWithIntersects
    , intersectionConsistentWithOverlappingBy
    , intersectionIsValidOrNothing
    , offsetByHalfDepthIsValidOrNothing
    , offsetByHalfHeightIsValidOrNothing
    , offsetByHalfWidthIsValidOrNothing
    , offsetResultIsValidOrNothing
    , overlappingBoxesCannotBySeparated
    , overlappingByDetectsIntersection
    , separatedBoxesCannotBeMadeToOverlap
    , separationIsCorrectForDiagonallyDisplacedBoxes
    , separationIsCorrectForHorizontallyDisplacedBoxes
    , separationIsCorrectForVerticallyDisplacedBoxes
    )

import BoundingBox3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Quantity
import Test exposing (Test)
import Vector3d


intersectionConsistentWithIntersects : Test
intersectionConsistentWithIntersects =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "'intersection' is consistent with 'intersects'"
        (\first second ->
            let
                intersects =
                    BoundingBox3d.intersects first second

                intersection =
                    BoundingBox3d.intersection first second
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
    Test.fuzz2
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "'intersection' is consistent with 'overlappingBy'"
        (\first second ->
            let
                overlapping =
                    BoundingBox3d.overlappingBy GT Quantity.zero first second

                intersection =
                    BoundingBox3d.intersection first second

                intersectionDimensions =
                    Maybe.map BoundingBox3d.dimensions intersection
            in
            case ( overlapping, intersectionDimensions ) of
                ( True, Just ( length, width, height ) ) ->
                    if length == Quantity.zero then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection length is 0"
                            )

                    else if width == Quantity.zero then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection width is 0"
                            )

                    else if height == Quantity.zero then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection height is 0"
                            )

                    else
                        Expect.pass

                ( False, Nothing ) ->
                    Expect.pass

                ( True, Nothing ) ->
                    Expect.fail
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " considered to strictly overlap, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just ( length, width, height ) ) ->
                    if
                        (length == Quantity.zero)
                            || (height == Quantity.zero)
                            || (width == Quantity.zero)
                    then
                        Expect.pass

                    else
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " not considered to strictly overlap, "
                                ++ "but have valid intersection "
                                ++ "with non-zero dimensions "
                                ++ Debug.toString intersectionDimensions
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
                    Expect.validBoundingBox3d result
        )


boxContainsOwnCenterPoint : Test
boxContainsOwnCenterPoint =
    Test.fuzz Fuzz.boundingBox3d
        "a bounding box contains its own center point"
        (\box ->
            let
                centerPoint =
                    BoundingBox3d.centerPoint box
            in
            Expect.true "bounding box does not contain its own center point"
                (BoundingBox3d.contains centerPoint box)
        )


overlappingByDetectsIntersection : Test
overlappingByDetectsIntersection =
    Test.fuzz2
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "overlappingBy LT 0 detects non-intersecting boxes"
        (\firstBox secondBox ->
            case BoundingBox3d.intersection firstBox secondBox of
                Just intersectionBox ->
                    Expect.false "intersecting boxes should overlap by at least 0"
                        (BoundingBox3d.overlappingBy LT Quantity.zero firstBox secondBox)

                Nothing ->
                    Expect.true "non-intersecting boxes should overlap by less than 0"
                        (BoundingBox3d.overlappingBy LT Quantity.zero firstBox secondBox)
        )


overlappingBoxesCannotBySeparated : Test
overlappingBoxesCannotBySeparated =
    Test.fuzz3
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        Fuzz.vector3d
        "boxes overlapping by greater than a distance cannot be separated by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector3d.length displacement
            in
            if BoundingBox3d.overlappingBy GT tolerance firstBox secondBox then
                BoundingBox3d.translateBy displacement firstBox
                    |> BoundingBox3d.intersects secondBox
                    |> Expect.true "displaced box should still intersect the other box"

            else
                Expect.pass
        )


separatedBoxesCannotBeMadeToOverlap : Test
separatedBoxesCannotBeMadeToOverlap =
    Test.fuzz3
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        Fuzz.vector3d
        "boxes separated by greater than a distance cannot be made to overlap by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector3d.length displacement
            in
            if BoundingBox3d.separatedBy GT tolerance firstBox secondBox then
                BoundingBox3d.translateBy displacement firstBox
                    |> BoundingBox3d.intersects secondBox
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
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 0
                        , minY = Quantity.float 0
                        , minZ = Quantity.float 0
                        , maxX = Quantity.float 1
                        , maxY = Quantity.float 1
                        , maxZ = Quantity.float 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 2
                        , minY = Quantity.float 0
                        , minZ = Quantity.float 0
                        , maxX = Quantity.float 3
                        , maxY = Quantity.float 1
                        , maxZ = Quantity.float 1
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 1) secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox3d.separatedBy GT (Quantity.float 0.5) secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT (Quantity.float 0) secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT (Quantity.float -1) secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox3d.separatedBy LT (Quantity.float 2) secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 2) secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox3d.separatedBy GT (Quantity.float 1) secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox3d.separatedBy LT (Quantity.float 1) secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT (Quantity.float 0) secondBox
                    ]
        )


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for vertically displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 0
                        , minY = Quantity.float 0
                        , minZ = Quantity.float 0
                        , maxX = Quantity.float 1
                        , maxY = Quantity.float 1
                        , maxZ = Quantity.float 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 0
                        , minY = Quantity.float 0
                        , minZ = Quantity.float 2
                        , maxX = Quantity.float 1
                        , maxY = Quantity.float 1
                        , maxZ = Quantity.float 3
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 1) secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox3d.separatedBy GT (Quantity.float 0.5) secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT (Quantity.float 0) secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT (Quantity.float -1) secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox3d.separatedBy LT (Quantity.float 2) secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 2) secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox3d.separatedBy GT (Quantity.float 1) secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox3d.separatedBy LT (Quantity.float 1) secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT (Quantity.float 0) secondBox
                    ]
        )


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for diagonally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 0
                        , minY = Quantity.float 0
                        , minZ = Quantity.float 0
                        , maxX = Quantity.float 1
                        , maxY = Quantity.float 1
                        , maxZ = Quantity.float 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = Quantity.float 2
                        , minY = Quantity.float 3
                        , minZ = Quantity.float 3
                        , maxX = Quantity.float 4
                        , maxY = Quantity.float 5
                        , maxZ = Quantity.float 6
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 3"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 3) secondBox
                    , Expect.true "Expected separation to be greater than 2"
                        << BoundingBox3d.separatedBy GT (Quantity.float 2) secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT (Quantity.float 0) secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT (Quantity.float -1) secondBox
                    , Expect.true "Expected separation to be less than 4"
                        << BoundingBox3d.separatedBy LT (Quantity.float 4) secondBox
                    , Expect.false "Expected separation to not be equal to 4"
                        << BoundingBox3d.separatedBy EQ (Quantity.float 4) secondBox
                    , Expect.false "Expected separation to not be greater than 3"
                        << BoundingBox3d.separatedBy GT (Quantity.float 3) secondBox
                    , Expect.false "Expected separation to not be less than 3"
                        << BoundingBox3d.separatedBy LT (Quantity.float 3) secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT (Quantity.float 0) secondBox
                    ]
        )


containingPointsConsistentWithFromCorners : Test
containingPointsConsistentWithFromCorners =
    Test.fuzz2
        Fuzz.point3d
        Fuzz.point3d
        "'containingPoints' is consistent with 'from'"
        (\firstPoint secondPoint ->
            BoundingBox3d.containingPoints [ firstPoint, secondPoint ]
                |> Expect.equal
                    (Just (BoundingBox3d.from firstPoint secondPoint))
        )


containingPointsIsOrderIndependent : Test
containingPointsIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point3d)
        "'containingPoints' does not depend on input order"
        (\points ->
            BoundingBox3d.containingPoints (List.reverse points)
                |> Expect.equal (BoundingBox3d.containingPoints points)
        )


offsetResultIsValidOrNothing : Test
offsetResultIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.length
        "offsetBy returns either Nothing or Just a valid box"
        (\boundingBox offset ->
            case BoundingBox3d.offsetBy offset boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox3d result
        )


offsetByHalfWidthIsValidOrNothing : Test
offsetByHalfWidthIsValidOrNothing =
    Test.fuzz Fuzz.boundingBox3d
        "offsetBy returns either Nothing or Just a valid box when offseting by -width / 2"
        (\boundingBox ->
            let
                ( width, height, depth ) =
                    BoundingBox3d.dimensions boundingBox

                negativeHalfWidth =
                    Quantity.multiplyBy -0.5 width
            in
            case BoundingBox3d.offsetBy negativeHalfWidth boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox3d result
        )


offsetByHalfHeightIsValidOrNothing : Test
offsetByHalfHeightIsValidOrNothing =
    Test.fuzz Fuzz.boundingBox3d
        "offsetBy returns either Nothing or Just a valid box when offseting by -height / 2"
        (\boundingBox ->
            let
                ( width, height, depth ) =
                    BoundingBox3d.dimensions boundingBox

                negativeHalfHeight =
                    Quantity.multiplyBy -0.5 height
            in
            case BoundingBox3d.offsetBy negativeHalfHeight boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox3d result
        )


offsetByHalfDepthIsValidOrNothing : Test
offsetByHalfDepthIsValidOrNothing =
    Test.fuzz Fuzz.boundingBox3d
        "offsetBy returns either Nothing or Just a valid box when offseting by -depth / 2"
        (\boundingBox ->
            let
                ( width, height, depth ) =
                    BoundingBox3d.dimensions boundingBox

                negativeHalfDepth =
                    Quantity.multiplyBy -0.5 depth
            in
            case BoundingBox3d.offsetBy negativeHalfDepth boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox3d result
        )
