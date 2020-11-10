module Tests.Generic.Curve2d exposing
    ( GlobalCoordinates
    , LocalCoordinates
    , Operations
    , firstDerivative
    , transformations
    )

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Frame2d exposing (Frame2d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Test exposing (Test)
import Vector2d exposing (Vector2d)


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias Operations curve coordinates =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> Float -> Point2d Meters coordinates
    , firstDerivative : curve -> Float -> Vector2d Meters coordinates
    , scaleAbout : Point2d Meters coordinates -> Float -> curve -> curve
    , translateBy : Vector2d Meters coordinates -> curve -> curve
    , rotateAround : Point2d Meters coordinates -> Angle -> curve -> curve
    , mirrorAcross : Axis2d Meters coordinates -> curve -> curve
    }


transformations :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
transformations global local placeIn relativeTo =
    Test.describe "Transformations"
        [ Test.describe "scaleAbout"
            [ Test.fuzz3
                global.fuzzer
                (Fuzz.tuple ( Fuzz.point2d, Fuzz.scale ))
                Fuzz.parameterValue
                "position"
                (\curve ( basePoint, scale ) t ->
                    let
                        scaledCurve =
                            global.scaleAbout basePoint scale curve

                        originalPoint =
                            global.pointOn curve t

                        pointOnScaledCurve =
                            global.pointOn scaledCurve t

                        scaledPoint =
                            originalPoint |> Point2d.scaleAbout basePoint scale
                    in
                    pointOnScaledCurve |> Expect.point2d scaledPoint
                )
            , Test.fuzz3
                global.fuzzer
                (Fuzz.tuple ( Fuzz.point2d, Fuzz.scale ))
                Fuzz.parameterValue
                "firstDerivative"
                (\curve ( basePoint, scale ) t ->
                    let
                        scaledCurve =
                            global.scaleAbout basePoint scale curve

                        originalDerivative =
                            global.firstDerivative curve t

                        derivativeOfScaledCurve =
                            global.firstDerivative scaledCurve t

                        scaledDerivative =
                            originalDerivative |> Vector2d.scaleBy scale
                    in
                    derivativeOfScaledCurve |> Expect.vector2d scaledDerivative
                )
            ]
        , Test.fuzz3
            global.fuzzer
            Fuzz.vector2d
            Fuzz.parameterValue
            "translateBy"
            (\curve displacement t ->
                let
                    translatedCurve =
                        global.translateBy displacement curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnTranslatedCurve =
                        global.pointOn translatedCurve t

                    translatedPoint =
                        originalPoint |> Point2d.translateBy displacement
                in
                pointOnTranslatedCurve |> Expect.point2d translatedPoint
            )
        , Test.fuzz3
            global.fuzzer
            (Fuzz.tuple
                ( Fuzz.point2d
                , Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))
                )
            )
            Fuzz.parameterValue
            "rotateAround"
            (\curve ( centerPoint, angle ) t ->
                let
                    rotatedCurve =
                        global.rotateAround centerPoint angle curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnRotatedCurve =
                        global.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint |> Point2d.rotateAround centerPoint angle
                in
                pointOnRotatedCurve |> Expect.point2d rotatedPoint
            )
        , Test.fuzz3
            global.fuzzer
            Fuzz.axis2d
            Fuzz.parameterValue
            "mirrorAcross"
            (\curve axis t ->
                let
                    mirroredCurve =
                        global.mirrorAcross axis curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnMirroredCurve =
                        global.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Point2d.mirrorAcross axis
                in
                pointOnMirroredCurve |> Expect.point2d mirroredPoint
            )
        , Test.fuzz3
            global.fuzzer
            Fuzz.frame2d
            Fuzz.parameterValue
            "relativeTo"
            (\globalCurve frame t ->
                let
                    localCurve =
                        relativeTo frame globalCurve

                    originalPoint =
                        global.pointOn globalCurve t

                    pointOnLocalCurve =
                        local.pointOn localCurve t

                    localPoint =
                        originalPoint |> Point2d.relativeTo frame
                in
                pointOnLocalCurve |> Expect.point2d localPoint
            )
        , Test.fuzz3
            local.fuzzer
            Fuzz.frame2d
            Fuzz.parameterValue
            "placeIn"
            (\localCurve frame t ->
                let
                    globalCurve =
                        placeIn frame localCurve

                    originalPoint =
                        local.pointOn localCurve t

                    pointOnGlobalCurve =
                        global.pointOn globalCurve t

                    globalPoint =
                        originalPoint |> Point2d.placeIn frame
                in
                pointOnGlobalCurve |> Expect.point2d globalPoint
            )
        ]


firstDerivative : Operations curve GlobalCoordinates -> Test
firstDerivative operations =
    Test.fuzz2
        operations.fuzzer
        Fuzz.parameterValue
        "Analytical first derivative matches numerical"
        (\curve t ->
            let
                analyticalDerivative =
                    operations.firstDerivative curve t

                numericalDerivative =
                    Vector2d.from
                        (operations.pointOn curve (t - 1.0e-6))
                        (operations.pointOn curve (t + 1.0e-6))
                        |> Vector2d.scaleBy 5.0e5
            in
            analyticalDerivative
                |> Expect.vector2dWithin (Length.meters 1.0e-6) numericalDerivative
        )
