{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Vector2d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import Test exposing (Test, fuzz, describe)
import Expect
import Test.Runner.Html exposing (run)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Compare as Compare
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect


jsonRoundTrips : Test
jsonRoundTrips =
    fuzz Fuzz.vector2d
        "JSON conversion round-trips properly"
        (\vector ->
            vector
                |> Encode.vector2d
                |> decodeValue Decode.vector2d
                |> Expect.equal (Ok vector)
        )


recordConversionRoundTrips : Test
recordConversionRoundTrips =
    fuzz Fuzz.vector2d
        "Record conversion round-trips properly"
        (\vector ->
            vector
                |> Vector2d.toRecord
                |> Vector2d.fromRecord
                |> Expect.equal vector
        )


perpendicularVectorIsPerpendicular : Test
perpendicularVectorIsPerpendicular =
    fuzz Fuzz.vector2d
        "perpendicularTo actually returns a perpendicular vector"
        (\vector ->
            vector
                |> Vector2d.perpendicularTo
                |> Vector2d.dotProduct vector
                |> Expect.scalar 0
        )


suite : Test
suite =
    describe "OpenSolid.Core.Vector2d"
        [ jsonRoundTrips
        , recordConversionRoundTrips
        , perpendicularVectorIsPerpendicular
        ]


main =
    run suite
