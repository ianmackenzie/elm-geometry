module Direction2d
    exposing
        ( angleFromAndEqualWithinAreConsistent
        , angleFromAndRotateByAreConsistent
        , jsonRoundTrips
        )

import Expect
import Generic
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.direction2d
        Encode.direction2d
        Decode.direction2d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    abs (Direction2d.angleFrom firstDirection secondDirection)
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction2d.equalWithin (angle + 1.0e-12)
                    firstDirection
                    secondDirection
                )
        )


angleFromAndRotateByAreConsistent : Test
angleFromAndRotateByAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and rotateBy are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction2d.angleFrom firstDirection secondDirection
            in
            firstDirection
                |> Direction2d.rotateBy angle
                |> Expect.direction2d secondDirection
        )
