--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Generic exposing (jsonRoundTrips, conversionRoundTrips)

import Test exposing (Test)
import Fuzz exposing (Fuzzer)
import Expect
import Json.Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)


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
