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
    , poissonDiskSamplingTests
    , separatedBoxesCannotBeMadeToOverlap
    , separationIsCorrectForDiagonallyDisplacedBoxes
    , separationIsCorrectForHorizontallyDisplacedBoxes
    , separationIsCorrectForVerticallyDisplacedBoxes
    , unionContainsInputs
    )

import Array
import BoundingBox2d
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d)
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity
import Random
import Shrink
import Test exposing (Test)
import Triangle2d exposing (Triangle2d)
import Vector2d


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
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'overlappingByAtLeast'"
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
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "union of two boxes contains both input boxes"
        (\first second ->
            let
                union =
                    BoundingBox2d.union first second

                isContained =
                    BoundingBox2d.isContainedIn union
            in
            Expect.true "Bounding box union does not contain both inputs"
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
                    Expect.validBoundingBox2d result
        )


boxContainsOwnCenterPoint : Test
boxContainsOwnCenterPoint =
    Test.fuzz Fuzz.boundingBox2d
        "a bounding box contains its own center point"
        (\box ->
            let
                centerPoint =
                    BoundingBox2d.centerPoint box
            in
            Expect.true "bounding box does not contain its own center point"
                (BoundingBox2d.contains centerPoint box)
        )


overlappingByDetectsIntersection : Test
overlappingByDetectsIntersection =
    Test.fuzz2
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "overlappingByAtLeast detects non-intersecting boxes"
        (\firstBox secondBox ->
            case BoundingBox2d.intersection firstBox secondBox of
                Just intersectionBox ->
                    Expect.true "intersecting boxes should overlap by at least 0"
                        (BoundingBox2d.overlappingByAtLeast Quantity.zero
                            firstBox
                            secondBox
                        )

                Nothing ->
                    Expect.false "non-intersecting boxes should overlap by less than 0"
                        (BoundingBox2d.overlappingByAtLeast Quantity.zero
                            firstBox
                            secondBox
                        )
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
            if
                BoundingBox2d.overlappingByAtLeast tolerance
                    firstBox
                    secondBox
            then
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
            if
                BoundingBox2d.separatedByAtLeast tolerance
                    firstBox
                    secondBox
            then
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
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0.5)
                            secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0)
                            secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float -1)
                            secondBox
                    , Expect.true "Expected separation to be greater than 0.99"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0.99)
                            secondBox
                    , Expect.false "Expected separation to not be greater than 1.01"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 1.01)
                            secondBox
                    ]
        )


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for vertically displaced boxes"
        (\_ ->
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
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0.5)
                            secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0)
                            secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float -1)
                            secondBox
                    , Expect.true "Expected separation to be greater than 0.99"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0.99)
                            secondBox
                    , Expect.false "Expected separation to not be greater than 1.01"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 1.01)
                            secondBox
                    ]
        )


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for diagonally displaced boxes"
        (\_ ->
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
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be greater than 4"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 4)
                            secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 0)
                            secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float -1)
                            secondBox
                    , Expect.true "Expected separation to be greater than 4.99"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 4.99)
                            secondBox
                    , Expect.false "Expected separation to not be greater than 5.01"
                        << BoundingBox2d.separatedByAtLeast (Quantity.float 5.01)
                            secondBox
                    ]
        )


offsetResultIsValidOrNothing : Test
offsetResultIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.length
        "offsetBy returns either Nothing or Just a valid box"
        (\boundingBox offset ->
            case BoundingBox2d.offsetBy offset boundingBox of
                Nothing ->
                    Expect.pass

                Just result ->
                    Expect.validBoundingBox2d result
        )


offsetByHalfWidthIsValidOrNothing : Test
offsetByHalfWidthIsValidOrNothing =
    Test.fuzz Fuzz.boundingBox2d
        "offsetBy returns either Nothing or Just a valid box when offseting by -width / 2"
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
    Test.fuzz Fuzz.boundingBox2d
        "offsetBy returns either Nothing or Just a valid box when offseting by -height / 2"
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
    Test.fuzz2
        Fuzz.point2d
        Fuzz.point2d
        "'hullN' is consistent with 'from'"
        (\firstPoint secondPoint ->
            BoundingBox2d.hullN [ firstPoint, secondPoint ]
                |> Expect.equal
                    (Just (BoundingBox2d.from firstPoint secondPoint))
        )


hullNIsOrderIndependent : Test
hullNIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "'hullN' does not depend on input order"
        (\points ->
            BoundingBox2d.hullN (List.reverse points)
                |> Expect.equal (BoundingBox2d.hullN points)
        )


randomSeed : Fuzzer Random.Seed
randomSeed =
    Fuzz.custom Random.independentSeed Shrink.noShrink


poissonDiskSamplingTests : Test
poissonDiskSamplingTests =
    Test.describe "poissonDiskSamples"
        [ Test.fuzz2 randomSeed (Fuzz.intRange 20 100) "Produces within a reasonable bound of the desired count" <|
            \seed count ->
                let
                    bbox =
                        BoundingBox2d.from (Point2d.meters 5 5) (Point2d.meters 10 10)
                in
                Random.step (BoundingBox2d.poissonDiskSamples count bbox) seed
                    |> Tuple.first
                    |> List.length
                    |> Expect.all
                        [ Expect.atLeast (floor (toFloat count * 0.91))
                        , Expect.atMost (ceiling (toFloat count * 1.5))
                        ]
        , Test.fuzz2 randomSeed (Fuzz.intRange 20 50) "points are reasonably far apart" <|
            \seed count ->
                let
                    bbox =
                        BoundingBox2d.from (Point2d.meters 0 0) (Point2d.meters 10 10)

                    minDist =
                        Length.meters (sqrt (100 / (toFloat count * 1.5)))
                in
                case
                    Random.step (BoundingBox2d.poissonDiskSamples count bbox) seed
                        |> Tuple.first
                        |> Array.fromList
                        |> DelaunayTriangulation2d.fromPoints
                of
                    Err (DelaunayTriangulation2d.CoincidentVertices v1 v2) ->
                        Expect.fail <| "poissonDiskSamples produces coincident points " ++ Debug.toString v1 ++ " and " ++ Debug.toString v2

                    Ok triangulation ->
                        triangulation
                            |> DelaunayTriangulation2d.triangles
                            |> List.concatMap
                                (\t ->
                                    let
                                        ( e1, e2, e3 ) =
                                            Triangle2d.edges t
                                    in
                                    [ e1, e2, e3 ]
                                )
                            |> List.all
                                (\e ->
                                    LineSegment2d.length e |> Quantity.greaterThanOrEqualTo minDist
                                )
                            |> Expect.true "all points are at least minDist distant from each other"
        ]
