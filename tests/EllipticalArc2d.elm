module EllipticalArc2d exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Generic
import Generic.Curve2d
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import Test exposing (Test)


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
                            (Vector2d.with
                                { length = radius
                                , direction = startDirection
                                }
                            )
            in
            Arc2d.with
                { centerPoint = centerPoint
                , startPoint = startPoint
                , sweptAngle = sweptAngle
                }
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


curveConfig : Generic.Curve2d.Config EllipticalArc2d
curveConfig =
    { fuzzer = Fuzz.ellipticalArc2d
    , pointOn = EllipticalArc2d.pointOn
    , derivative = EllipticalArc2d.derivative
    }


scaling : Test
scaling =
    Generic.Curve2d.scaling curveConfig EllipticalArc2d.scaleAbout


translation : Test
translation =
    Generic.Curve2d.translation curveConfig EllipticalArc2d.translateBy


rotation : Test
rotation =
    Generic.Curve2d.rotation curveConfig EllipticalArc2d.rotateAround


localization : Test
localization =
    Generic.Curve2d.localization curveConfig EllipticalArc2d.relativeTo


globalization : Test
globalization =
    Generic.Curve2d.globalization curveConfig EllipticalArc2d.placeIn
