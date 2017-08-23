module Direction3d
    exposing
        ( angleFromAndEqualWithinAreConsistent
        , jsonRoundTrips
        , onIsSpecialCaseOfWith
        )

import Expect
import Generic
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.direction3d
        Encode.direction3d
        Decode.direction3d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction3d
        Fuzz.direction3d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction3d.angleFrom firstDirection secondDirection
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction3d.equalWithin (angle + 1.0e-12)
                    firstDirection
                    secondDirection
                )
        )


onIsSpecialCaseOfWith : Test
onIsSpecialCaseOfWith =
    Test.fuzz2 Fuzz.sketchPlane3d
        Fuzz.scalar
        "on is a special case of with"
        (\sketchPlane angle ->
            Direction3d.on sketchPlane angle
                |> Expect.direction3d
                    (Direction3d.with
                        { referencePlane = sketchPlane
                        , azimuth = angle
                        , elevation = 0
                        }
                    )
        )
