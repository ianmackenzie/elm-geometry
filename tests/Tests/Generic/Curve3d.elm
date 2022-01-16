module Tests.Generic.Curve3d exposing
    ( GlobalCoordinates
    , LocalCoordinates
    , Operations
    , projectInto
    , secondDerivativeBoundingBox
    , tests
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Frame3d exposing (Frame3d)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Length, Meters)
import LineSegment3d
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d
import Quantity
import Random exposing (Generator)
import SketchPlane3d exposing (SketchPlane3d)
import Test exposing (Test)
import Test.Check as Test
import Tests.Generic.Curve2d as Curve2d
import Vector3d exposing (Vector3d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias Operations curve coordinates =
    { generator : Generator curve
    , pointOn : curve -> Float -> Point3d Meters coordinates
    , boundingBox : curve -> BoundingBox3d Meters coordinates
    , firstDerivative : curve -> Float -> Vector3d Meters coordinates
    , firstDerivativeBoundingBox : curve -> VectorBoundingBox3d Meters coordinates
    , scaleAbout : Point3d Meters coordinates -> Float -> curve -> curve
    , translateBy : Vector3d Meters coordinates -> curve -> curve
    , rotateAround : Axis3d Meters coordinates -> Angle -> curve -> curve
    , mirrorAcross : Plane3d Meters coordinates -> curve -> curve
    , numApproximationSegments : Length -> curve -> Int
    }


tests :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
tests global local placeIn relativeTo =
    Test.describe "Generic 3D curve tests"
        [ transformations global local placeIn relativeTo
        , firstDerivative global
        , approximate global
        , boundingBox global
        , firstDerivativeBoundingBox global
        ]


transformations :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
transformations global local placeIn relativeTo =
    Test.describe "Transformations"
        [ Test.check3 "scaleAbout"
            global.generator
            (Random.map2 Tuple.pair Random.point3d Random.scale)
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
                        originalPoint |> Point3d.scaleAbout basePoint scale
                in
                pointOnScaledCurve |> Expect.point3d scaledPoint
            )
        , Test.check3 "translateBy"
            global.generator
            Random.vector3d
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
                        originalPoint |> Point3d.translateBy displacement
                in
                pointOnTranslatedCurve |> Expect.point3d translatedPoint
            )
        , Test.check3 "rotateAround"
            global.generator
            (Random.map2 Tuple.pair
                Random.axis3d
                (Random.map Angle.radians (Random.float (-2 * pi) (2 * pi)))
            )
            Random.parameterValue
            (\curve ( axis, angle ) t ->
                let
                    rotatedCurve =
                        global.rotateAround axis angle curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnRotatedCurve =
                        global.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint |> Point3d.rotateAround axis angle
                in
                pointOnRotatedCurve |> Expect.point3d rotatedPoint
            )
        , Test.check3 "mirrorAcross"
            global.generator
            Random.plane3d
            Random.parameterValue
            (\curve plane t ->
                let
                    mirroredCurve =
                        global.mirrorAcross plane curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnMirroredCurve =
                        global.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Point3d.mirrorAcross plane
                in
                pointOnMirroredCurve |> Expect.point3d mirroredPoint
            )
        , Test.check3 "relativeTo"
            global.generator
            Random.frame3d
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
                        originalPoint |> Point3d.relativeTo frame
                in
                pointOnLocalCurve |> Expect.point3d localPoint
            )
        , Test.check3 "placeIn"
            local.generator
            Random.frame3d
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
                        originalPoint |> Point3d.placeIn frame
                in
                pointOnGlobalCurve |> Expect.point3d globalPoint
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
                    Vector3d.from
                        (operations.pointOn curve (t - 1.0e-6))
                        (operations.pointOn curve (t + 1.0e-6))
                        |> Vector3d.scaleBy 5.0e5
            in
            analyticalDerivative
                |> Expect.vector3dWithin (Length.meters 1.0e-6) numericalDerivative
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
                    Polyline3d.segments (Polyline3d.fromVertices vertices)

                testPoints =
                    Parameter1d.midpoints numSegments (operations.pointOn curve)

                errors =
                    List.map2
                        (\segment testPoint ->
                            Point3d.distanceFrom testPoint (LineSegment3d.midpoint segment)
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
                |> Expect.point3dContainedIn (operations.boundingBox curve)
        )


firstDerivativeBoundingBox : Operations curve coordinates -> Test
firstDerivativeBoundingBox operations =
    Test.check2 "firstDerivativeBoundingBox"
        operations.generator
        (Random.float 0 1)
        (\curve parameterValue ->
            operations.firstDerivative curve parameterValue
                |> Expect.vector3dContainedIn (operations.firstDerivativeBoundingBox curve)
        )


secondDerivativeBoundingBox :
    { generator : Generator curve
    , secondDerivative : curve -> Float -> Vector3d Meters coordinates
    , secondDerivativeBoundingBox : curve -> VectorBoundingBox3d Meters coordinates
    }
    -> Test
secondDerivativeBoundingBox operations =
    Test.check2 "secondDerivativeBoundingBox"
        operations.generator
        (Random.float 0 1)
        (\curve parameterValue ->
            operations.secondDerivative curve parameterValue
                |> Expect.vector3dContainedIn (operations.secondDerivativeBoundingBox curve)
        )


projectInto : Operations curve coordinates -> (SketchPlane3d Meters coordinates { defines : coordinates2d } -> curve -> curve2d) -> Curve2d.Operations curve2d coordinates2d -> Test
projectInto operations project operations2d =
    Test.describe "projectInto"
        [ Test.check3 "pointOn"
            operations.generator
            Random.sketchPlane3d
            (Random.float 0 1)
            (\curve sketchPlane parameterValue ->
                let
                    curve2d =
                        project sketchPlane curve

                    point3d =
                        operations.pointOn curve parameterValue

                    point2d =
                        operations2d.pointOn curve2d parameterValue

                    projectedPoint =
                        Point3d.projectInto sketchPlane point3d
                in
                projectedPoint |> Expect.point2d point2d
            )
        , Test.check3 "firstDerivative"
            operations.generator
            Random.sketchPlane3d
            (Random.float 0 1)
            (\curve sketchPlane parameterValue ->
                let
                    curve2d =
                        project sketchPlane curve

                    derivative3d =
                        operations.firstDerivative curve parameterValue

                    derivative2d =
                        operations2d.firstDerivative curve2d parameterValue

                    projectedDerivative =
                        Vector3d.projectInto sketchPlane derivative3d
                in
                projectedDerivative |> Expect.vector2d derivative2d
            )
        ]
