module Test.FuzzN exposing (fuzz4, fuzz5)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


type alias Arguments4 a b c d =
    { argument1 : a
    , argument2 : b
    , argument3 : c
    , argument4 : d
    }


fuzz4 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> String -> (a -> b -> c -> d -> Expectation) -> Test
fuzz4 fuzzer1 fuzzer2 fuzzer3 fuzzer4 description expectation =
    Test.fuzz
        (Fuzz.map4 Arguments4 fuzzer1 fuzzer2 fuzzer3 fuzzer4)
        description
        (\{ argument1, argument2, argument3, argument4 } ->
            expectation argument1 argument2 argument3 argument4
        )


type alias Arguments5 a b c d e =
    { argument1 : a
    , argument2 : b
    , argument3 : c
    , argument4 : d
    , argument5 : e
    }


fuzz5 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> String -> (a -> b -> c -> d -> e -> Expectation) -> Test
fuzz5 fuzzer1 fuzzer2 fuzzer3 fuzzer4 fuzzer5 description expectation =
    Test.fuzz
        (Fuzz.map5 Arguments5 fuzzer1 fuzzer2 fuzzer3 fuzzer4 fuzzer5)
        description
        (\{ argument1, argument2, argument3, argument4, argument5 } ->
            expectation argument1 argument2 argument3 argument4 argument5
        )
