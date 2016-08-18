{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Vector2d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.vector2d Encode.vector2d Decode.vector2d


perpendicularVectorIsPerpendicular : Test
perpendicularVectorIsPerpendicular =
    Test.fuzz Fuzz.vector2d
        "perpendicularTo actually returns a perpendicular vector"
        (\vector ->
            vector
                |> Vector2d.perpendicularTo
                |> Vector2d.dotProduct vector
                |> Expect.approximately 0
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Vector2d"
        [ jsonRoundTrips
        , perpendicularVectorIsPerpendicular
        ]


main : Program Never
main =
    Html.run suite
