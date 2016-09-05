{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Frame3d exposing (suite)

import Test exposing (Test)
import Test.Runner.Html as Html
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Decode as Decode
import OpenSolid.Fuzz.Frame3d as Fuzz
import OpenSolid.Expect as Expect
import OpenSolid.Expect.Frame3d as Expect
import Generic


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.frame3d Encode.frame3d Decode.frame3d


frameDirectionsAreOrthonormal : Test
frameDirectionsAreOrthonormal =
    Test.fuzz Fuzz.frame3d
        "Frame3d basis directions are orthonormal"
        (\frame ->
            let
                xDirection =
                    Frame3d.xDirection frame

                yDirection =
                    Frame3d.yDirection frame

                zDirection =
                    Frame3d.zDirection frame

                tripleProduct =
                    Direction3d.crossProduct xDirection yDirection
                        |> Vector3d.componentIn zDirection
            in
                Expect.approximately 1 tripleProduct
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Frame3d"
        [ jsonRoundTrips
        , frameDirectionsAreOrthonormal
        ]


main : Program Never
main =
    Html.run suite
