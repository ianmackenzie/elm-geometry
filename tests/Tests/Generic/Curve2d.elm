module Tests.Generic.Curve2d exposing
    ( GlobalCoordinates
    , LocalCoordinates
    , Operations
    , secondDerivativeBoundingBox
    , tests
    )

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Length, Meters)
import LineSegment2d
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Random exposing (Generator)
import Test exposing (Test)
import Test.Check as Test
import Vector2d exposing (Vector2d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias Operations curve coordinates =
    { generator : Generator curve
    , pointOn : curve -> Float -> Point2d Meters coordinates
    , boundingBox : curve -> BoundingBox2d Meters coordinates
    , firstDerivative : curve -> Float -> Vector2d Meters coordinates
    , firstDerivativeBoundingBox : curve -> VectorBoundingBox2d Meters coordinates
    , scaleAbout : Point2d Meters coordinates -> Float -> curve -> curve
    , translateBy : Vector2d Meters coordinates -> curve -> curve
    , rotateAround : Point2d Meters coordinates -> Angle -> curve -> curve
    , mirrorAcross : Axis2d Meters coordinates -> curve -> curve
    , numApproximationSegments : Length -> curve -> Int
    }


tests :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
tests global local placeIn relativeTo =
    Test.describe "Generic 2D curve tests"
        [ transformations global local placeIn relativeTo
        , firstDerivative global
        , approximate global
        , boundingBox global
        , firstDerivativeBoundingBox global
        ]


transformations :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame2d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
transformations global local placeIn relativeTo =
    Test.describe "Transformations"
        [ Test.describe "scaleAbout"
            [ Test.check3 "position"
                global.generator
                (Random.map2 Tuple.pair Random.point2d Random.scale)
                Random.parameterValue
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
            , Test.check3 "firstDerivative"
                global.generator
                (Random.map2 Tuple.pair Random.point2d Random.scale)
                Random.parameterValue
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
        , Test.check3 "translateBy"
            global.generator
            Random.vector2d
            Random.parameterValue
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
        , Test.check3 "rotateAround"
            global.generator
            (Random.map2 Tuple.pair
                Random.point2d
                (Random.map Angle.radians (Random.float (-2 * pi) (2 * pi)))
            )
            Random.parameterValue
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
        , Test.check3 "mirrorAcross"
            global.generator
            Random.axis2d
            Random.parameterValue
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
        , Test.check3 "relativeTo"
            global.generator
            Random.frame2d
            Random.parameterValue
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
        , Test.check3 "placeIn"
            local.generator
            Random.frame2d
            Random.parameterValue
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
    Test.check2 "Analytical first derivative matches numerical"
        operations.generator
        Random.parameterValue
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


approximate : Operations curve GlobalCoordinates -> Test
approximate operations =
    Test.check "approximate has desired accuracy"
        operations.generator
        (\curve ->
            let
                tolerance =
                    Length.centimeters 1

                numSegments =
                    operations.numApproximationSegments tolerance curve

                vertices =
                    Parameter1d.steps numSegments (operations.pointOn curve)

                segments =
                    Polyline2d.segments (Polyline2d.fromVertices vertices)

                testPoints =
                    Parameter1d.midpoints numSegments (operations.pointOn curve)

                errors =
                    List.map2
                        (\segment testPoint ->
                            Point2d.distanceFrom testPoint (LineSegment2d.midpoint segment)
                        )
                        segments
                        testPoints

                maxError =
                    List.foldl Quantity.max Quantity.zero errors
            in
            maxError |> Expect.quantityLessThan tolerance
        )


boundingBox : Operations curve coordinates -> Test
boundingBox operations =
    Test.check2 "boundingBox"
        operations.generator
        Random.parameterValue
        (\curve t ->
            operations.pointOn curve t
                |> Expect.point2dContainedIn (operations.boundingBox curve)
        )


firstDerivativeBoundingBox : Operations curve coordinates -> Test
firstDerivativeBoundingBox operations =
    Test.check2 "firstDerivativeBoundingBox"
        operations.generator
        (Random.float 0 1)
        (\curve parameterValue ->
            operations.firstDerivative curve parameterValue
                |> Expect.vector2dContainedIn (operations.firstDerivativeBoundingBox curve)
        )


secondDerivativeBoundingBox :
    { generator : Generator curve
    , secondDerivative : curve -> Float -> Vector2d Meters coordinates
    , secondDerivativeBoundingBox : curve -> VectorBoundingBox2d Meters coordinates
    }
    -> Test
secondDerivativeBoundingBox operations =
    Test.check2 "secondDerivativeBoundingBox"
        operations.generator
        (Random.float 0 1)
        (\curve parameterValue ->
            operations.secondDerivative curve parameterValue
                |> Expect.vector2dContainedIn (operations.secondDerivativeBoundingBox curve)
        )
