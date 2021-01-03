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
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Shrink exposing (Shrinker)
import Test exposing (Test)


toFuzzer : Generator a -> Fuzzer a
toFuzzer generator =
    Fuzz.custom generator Shrink.noShrink


check : String -> Generator a -> (a -> Expectation) -> Test
check description generator expectation =
    Test.fuzz (toFuzzer generator) description expectation


check2 : String -> Generator a -> Generator b -> (a -> b -> Expectation) -> Test
check2 description firstGenerator secondGenerator expectation =
    Test.fuzz2
        (toFuzzer firstGenerator)
        (toFuzzer secondGenerator)
        description
        expectation


check3 : String -> Generator a -> Generator b -> Generator c -> (a -> b -> c -> Expectation) -> Test
check3 description firstGenerator secondGenerator thirdGenerator expectation =
    Test.fuzz3
        (toFuzzer firstGenerator)
        (toFuzzer secondGenerator)
        (toFuzzer thirdGenerator)
        description
        expectation


type FourValues a b c d
    = FourValues a b c d


check4 :
    String
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> (a -> b -> c -> d -> Expectation)
    -> Test
check4 description firstGenerator secondGenerator thirdGenerator fourthGenerator expectation =
    Test.fuzz
        (Fuzz.map4 FourValues
            (toFuzzer firstGenerator)
            (toFuzzer secondGenerator)
            (toFuzzer thirdGenerator)
            (toFuzzer fourthGenerator)
        )
        description
        (\(FourValues firstValue secondValue thirdValue fourthValue) ->
            expectation firstValue secondValue thirdValue fourthValue
        )


type FiveValues a b c d e
    = FiveValues a b c d e


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
    Test.fuzz
        (Fuzz.map5 FiveValues
            (toFuzzer firstGenerator)
            (toFuzzer secondGenerator)
            (toFuzzer thirdGenerator)
            (toFuzzer fourthGenerator)
            (toFuzzer fifthGenerator)
        )
        description
        (\(FiveValues firstValue secondValue thirdValue fourthValue fifthValue) ->
            expectation firstValue secondValue thirdValue fourthValue fifthValue
        )


type SixValues a b c d e f
    = SixValues a b c d e f


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
    Test.fuzz
        (Fuzz.map SixValues (toFuzzer firstGenerator)
            |> Fuzz.andMap (toFuzzer secondGenerator)
            |> Fuzz.andMap (toFuzzer thirdGenerator)
            |> Fuzz.andMap (toFuzzer fourthGenerator)
            |> Fuzz.andMap (toFuzzer fifthGenerator)
            |> Fuzz.andMap (toFuzzer sixthGenerator)
        )
        description
        (\(SixValues firstValue secondValue thirdValue fourthValue fifthValue sixthValue) ->
            expectation firstValue secondValue thirdValue fourthValue fifthValue sixthValue
        )


type SevenValues a b c d e f g
    = SevenValues a b c d e f g


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
    Test.fuzz
        (Fuzz.map SevenValues (toFuzzer firstGenerator)
            |> Fuzz.andMap (toFuzzer secondGenerator)
            |> Fuzz.andMap (toFuzzer thirdGenerator)
            |> Fuzz.andMap (toFuzzer fourthGenerator)
            |> Fuzz.andMap (toFuzzer fifthGenerator)
            |> Fuzz.andMap (toFuzzer sixthGenerator)
            |> Fuzz.andMap (toFuzzer seventhGenerator)
        )
        description
        (\(SevenValues firstValue secondValue thirdValue fourthValue fifthValue sixthValue seventhValue) ->
            expectation firstValue secondValue thirdValue fourthValue fifthValue sixthValue seventhValue
        )


type EightValues a b c d e f g h
    = EightValues a b c d e f g h


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
    Test.fuzz
        (Fuzz.map EightValues (toFuzzer firstGenerator)
            |> Fuzz.andMap (toFuzzer secondGenerator)
            |> Fuzz.andMap (toFuzzer thirdGenerator)
            |> Fuzz.andMap (toFuzzer fourthGenerator)
            |> Fuzz.andMap (toFuzzer fifthGenerator)
            |> Fuzz.andMap (toFuzzer sixthGenerator)
            |> Fuzz.andMap (toFuzzer seventhGenerator)
            |> Fuzz.andMap (toFuzzer eigthGenerator)
        )
        description
        (\(EightValues firstValue secondValue thirdValue fourthValue fifthValue sixthValue seventhValue eighthValue) ->
            expectation firstValue secondValue thirdValue fourthValue fifthValue sixthValue seventhValue eighthValue
        )
