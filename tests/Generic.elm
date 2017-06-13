module Generic
    exposing
        ( conversionRoundTrips
        , jsonRoundTrips
        )

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Test exposing (Test)


jsonRoundTrips : Fuzzer a -> (a -> Value) -> Decoder a -> Test
jsonRoundTrips fuzzer encode decoder =
    Test.fuzz fuzzer
        "JSON conversion round-trips properly"
        (\value ->
            value
                |> encode
                |> Decode.decodeValue decoder
                |> Expect.equal (Ok value)
        )


conversionRoundTrips : String -> Fuzzer a -> (a -> b) -> (b -> a) -> Test
conversionRoundTrips description fuzzer to from =
    Test.fuzz fuzzer
        (description ++ " conversion round-trips properly")
        (\value -> value |> to |> from |> Expect.equal value)
