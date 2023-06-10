module Test.Check exposing
    ( check
    , check2
    , check3
    , check4
    , check5
    , check6
    , check7
    , check8
    )

import Expect exposing (Expectation)
import Fuzz
import Random exposing (Generator)
import Test exposing (Test)


checkImpl : String -> Generator Expectation -> Test
checkImpl description generator =
    Test.fuzz (Fuzz.fromGenerator generator) description identity


check : String -> Generator a -> (a -> Expectation) -> Test
check description generator expectation =
    checkImpl description (Random.map expectation generator)


check2 : String -> Generator a -> Generator b -> (a -> b -> Expectation) -> Test
check2 description firstGenerator secondGenerator expectation =
    checkImpl description <|
        Random.map2
            expectation
            firstGenerator
            secondGenerator


check3 : String -> Generator a -> Generator b -> Generator c -> (a -> b -> c -> Expectation) -> Test
check3 description firstGenerator secondGenerator thirdGenerator expectation =
    checkImpl description <|
        Random.map3 expectation
            firstGenerator
            secondGenerator
            thirdGenerator


check4 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> (a -> b -> c -> d -> Expectation)
    -> Test
check4 description firstGenerator secondGenerator thirdGenerator fourthGenerator expectation =
    checkImpl description <|
        Random.map4 expectation
            firstGenerator
            secondGenerator
            thirdGenerator
            fourthGenerator


check5 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> Generator e
    -> (a -> b -> c -> d -> e -> Expectation)
    -> Test
check5 description firstGenerator secondGenerator thirdGenerator fourthGenerator fifthGenerator expectation =
    checkImpl description <|
        Random.map5 expectation
            firstGenerator
            secondGenerator
            thirdGenerator
            fourthGenerator
            fifthGenerator


andMap : Generator a -> Generator (a -> b) -> Generator b
andMap valueGenerator functionGenerator =
    Random.map2 (|>) valueGenerator functionGenerator


check6 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> Generator e
    -> Generator f
    -> (a -> b -> c -> d -> e -> f -> Expectation)
    -> Test
check6 description firstGenerator secondGenerator thirdGenerator fourthGenerator fifthGenerator sixthGenerator expectation =
    checkImpl description
        (Random.constant expectation
            |> andMap firstGenerator
            |> andMap secondGenerator
            |> andMap thirdGenerator
            |> andMap fourthGenerator
            |> andMap fifthGenerator
            |> andMap sixthGenerator
        )


check7 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> Generator e
    -> Generator f
    -> Generator g
    -> (a -> b -> c -> d -> e -> f -> g -> Expectation)
    -> Test
check7 description firstGenerator secondGenerator thirdGenerator fourthGenerator fifthGenerator sixthGenerator seventhGenerator expectation =
    checkImpl description
        (Random.constant expectation
            |> andMap firstGenerator
            |> andMap secondGenerator
            |> andMap thirdGenerator
            |> andMap fourthGenerator
            |> andMap fifthGenerator
            |> andMap sixthGenerator
            |> andMap seventhGenerator
        )


check8 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> Generator e
    -> Generator f
    -> Generator g
    -> Generator h
    -> (a -> b -> c -> d -> e -> f -> g -> h -> Expectation)
    -> Test
check8 description firstGenerator secondGenerator thirdGenerator fourthGenerator fifthGenerator sixthGenerator seventhGenerator eigthGenerator expectation =
    checkImpl description
        (Random.constant expectation
            |> andMap firstGenerator
            |> andMap secondGenerator
            |> andMap thirdGenerator
            |> andMap fourthGenerator
            |> andMap fifthGenerator
            |> andMap sixthGenerator
            |> andMap seventhGenerator
            |> andMap eigthGenerator
        )
