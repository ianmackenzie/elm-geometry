{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Point3d exposing (suite)

import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test)
import Expect
import Test.Runner.Html as Html
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import OpenSolid.Core.Test.Compare as Compare
import OpenSolid.Core.Test.Fuzz as Fuzz
import OpenSolid.Core.Test.Expect as Expect


rotationAboutAxisPreservesDistance : Test
rotationAboutAxisPreservesDistance =
    let
        description =
            "Rotation about axis preserves distance along that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.signedDistanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.signedDistanceAlong axis rotatedPoint
            in
                Expect.approximately distance rotatedDistance
    in
        Test.fuzz3 Fuzz.point3d Fuzz.axis3d Fuzz.scalar description expectation


jsonRoundTrips : Test
jsonRoundTrips =
    Test.fuzz Fuzz.point3d
        "JSON conversion round-trips properly"
        (\point ->
            point
                |> Encode.point3d
                |> Decode.decodeValue Decode.point3d
                |> Expect.equal (Ok point)
        )


recordConversionRoundTrips : Test
recordConversionRoundTrips =
    Test.fuzz Fuzz.point3d
        "Record conversion round-trips properly"
        (\point ->
            point
                |> Point3d.toRecord
                |> Point3d.fromRecord
                |> Expect.equal point
        )


suite : Test
suite =
    Test.describe "OpenSolid.Core.Point3d"
        [ rotationAboutAxisPreservesDistance
        , jsonRoundTrips
        , recordConversionRoundTrips
        ]


main : Program Never
main =
    Html.run suite
