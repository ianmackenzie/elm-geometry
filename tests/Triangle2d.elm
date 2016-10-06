{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Triangle2d exposing (suite)

import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Fuzz as Fuzz
import OpenSolid.Core.Expect as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.triangle2d Encode.triangle2d Decode.triangle2d


triangleContainsOwnCentroid : Test
triangleContainsOwnCentroid =
    Test.fuzz Fuzz.triangle2d
        "triangle contains its own centroid"
        (\triangle ->
            let
                centroid =
                    Triangle2d.centroid triangle
            in
                Expect.true "triangle did not contain its own centroid"
                    (Triangle2d.contains centroid triangle)
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Triangle2d"
        [ jsonRoundTrips
        , triangleContainsOwnCentroid
        ]


main : Program Never
main =
    Html.run suite
