module Tests.EllipticalArc2d exposing (..)

import Arc2d exposing (Arc2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Decode as Decode
import Geometry.Encode as Encode
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Test exposing (Test)
import Tests.Generic as Generic
import Tests.Generic.Curve2d
import Vector2d


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.ellipticalArc2d
        Encode.ellipticalArc2d
        Decode.ellipticalArc2d


reproducibleArc : Fuzzer Arc2d
reproducibleArc =
    Fuzz.map4
        (\centerPoint startDirection radius sweptAngle ->
            let
                startPoint =
                    centerPoint
                        |> Point2d.translateBy
                            (Vector2d.withLength radius startDirection)
            in
            startPoint |> Arc2d.sweptAround centerPoint sweptAngle
        )
        Fuzz.point2d
        Fuzz.direction2d
        (Fuzz.floatRange 0.1 10)
        (Fuzz.oneOf
            [ Fuzz.floatRange (degrees 1) (degrees 179)
            , Fuzz.floatRange (degrees 181) (degrees 359)
            , Fuzz.floatRange (degrees -179) (degrees -1)
            , Fuzz.floatRange (degrees -359) (degrees -181)
            ]
        )


fromEndpointsReplicatesArc : Test
fromEndpointsReplicatesArc =
    Test.fuzz2
        reproducibleArc
        Fuzz.direction2d
        "fromEndpoints accurately replicates circular arcs"
        (\arc xDirection ->
            let
                radius =
                    Arc2d.radius arc

                arcSweptAngle =
                    Arc2d.sweptAngle arc

                sweptAngle =
                    if arcSweptAngle >= pi then
                        EllipticalArc2d.largePositive
                    else if arcSweptAngle >= 0 then
                        EllipticalArc2d.smallPositive
                    else if arcSweptAngle >= -pi then
                        EllipticalArc2d.smallNegative
                    else
                        EllipticalArc2d.largeNegative

                result =
                    EllipticalArc2d.fromEndpoints
                        { startPoint = Arc2d.startPoint arc
                        , endPoint = Arc2d.endPoint arc
                        , xRadius = radius
                        , yRadius = radius
                        , xDirection = xDirection
                        , sweptAngle = sweptAngle
                        }
            in
            case result of
                Nothing ->
                    Expect.fail "fromEndpoints could not reproduce arc"

                Just ellipticalArc ->
                    EllipticalArc2d.centerPoint ellipticalArc
                        |> Expect.point2d (Arc2d.centerPoint arc)
        )


curveConfig : Tests.Generic.Curve2d.Config EllipticalArc2d
curveConfig =
    { fuzzer = Fuzz.ellipticalArc2d
    , pointOn = EllipticalArc2d.pointOn
    , derivative = EllipticalArc2d.derivative
    }


scaling : Test
scaling =
    Tests.Generic.Curve2d.scaling curveConfig EllipticalArc2d.scaleAbout


translation : Test
translation =
    Tests.Generic.Curve2d.translation curveConfig EllipticalArc2d.translateBy


rotation : Test
rotation =
    Tests.Generic.Curve2d.rotation curveConfig EllipticalArc2d.rotateAround


localization : Test
localization =
    Tests.Generic.Curve2d.localization curveConfig EllipticalArc2d.relativeTo


globalization : Test
globalization =
    Tests.Generic.Curve2d.globalization curveConfig EllipticalArc2d.placeIn
