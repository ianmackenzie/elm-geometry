module Body3d exposing
    ( block, sphere, cylinder, cone
    , extrusion, revolution
    )

{-|

@docs block, sphere, cylinder, cone

@docs extrusion, revolution

@docs approximate

-}

import Angle exposing (Angle, Radians)
import Arc3d
import Array exposing (Array)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Circle2d exposing (Circle2d)
import Circle3d
import Cone3d exposing (Cone3d)
import Curve
import Curve2d exposing (Curve2d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline3d
import Quantity exposing (Quantity(..), Squared, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Rectangle2d exposing (Rectangle2d)
import Region2d exposing (Region2d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type alias Body3d units coordinates =
    Types.Body3d units coordinates


block : Block3d units coordinates -> Body3d units coordinates
block givenBlock =
    Types.RectangularBody givenBlock


sphere : Sphere3d units coordinates -> Body3d units coordinates
sphere givenSphere =
    Types.SphericalBody givenSphere


cylinder : Cylinder3d units coordinates -> Body3d units coordinates
cylinder givenCylinder =
    Types.CylindricalBody givenCylinder


cone : Cone3d units coordinates -> Body3d units coordinates
cone givenCone =
    Types.ConicalBody givenCone


extrusion :
    { sketchPlane : SketchPlane3d units coordinates { defines : sketchCoordinates }
    , profile : Region2d units sketchCoordinates
    , distance : Interval Float units
    }
    -> Body3d units coordinates
extrusion { sketchPlane, profile, distance } =
    Types.ExtrusionBody
        (SketchPlane3d.on sketchPlane Frame2d.atOrigin)
        (Region2d.placeIn Frame2d.atOrigin profile)
        distance


revolution :
    { sketchPlane : SketchPlane3d units coordinates { defines : sketchCoordinates }
    , profile : Region2d units sketchCoordinates
    , axis : Axis2d units sketchCoordinates
    , angle : Interval Float Radians
    }
    -> Body3d units coordinates
revolution { sketchPlane, profile, axis, angle } =
    Types.RevolutionBody
        (SketchPlane3d.on sketchPlane Frame2d.atOrigin)
        (Region2d.placeIn Frame2d.atOrigin profile)
        (Axis2d.placeIn Frame2d.atOrigin axis)
        angle


regionSamples : Quantity Float units -> Region2d units coordinates -> List (List ( Point2d units coordinates, Direction2d coordinates ))
regionSamples resolution givenRegion =
    case givenRegion of
        Types.EmptyRegion ->
            []

        Types.TriangularRegion givenTriangle ->
            let
                ( p1, p2, p3 ) =
                    Triangle2d.vertices givenTriangle
            in
            if Triangle2d.counterclockwiseArea givenTriangle |> Quantity.greaterThanOrEqualTo Quantity.zero then
                List.map (Curve2d.samples resolution)
                    [ Curve2d.lineSegment (LineSegment2d.from p1 p2)
                    , Curve2d.lineSegment (LineSegment2d.from p2 p3)
                    , Curve2d.lineSegment (LineSegment2d.from p3 p1)
                    ]

            else
                List.map (Curve2d.samples resolution)
                    [ Curve2d.lineSegment (LineSegment2d.from p1 p3)
                    , Curve2d.lineSegment (LineSegment2d.from p3 p2)
                    , Curve2d.lineSegment (LineSegment2d.from p2 p1)
                    ]

        Types.RectangularRegion givenRectangle ->
            let
                edges =
                    Rectangle2d.edges givenRectangle
            in
            if Frame2d.isRightHanded (Rectangle2d.axes givenRectangle) then
                List.map (Curve2d.lineSegment >> Curve2d.samples resolution) edges

            else
                List.reverse (List.map LineSegment2d.reverse edges)
                    |> List.map (Curve2d.lineSegment >> Curve2d.samples resolution)

        Types.CircularRegion givenCircle ->
            [ Curve2d.samples resolution (Curve2d.arc (Circle2d.toArc givenCircle)) ]

        Types.EllipticalRegion givenEllipse ->
            let
                ellipticalArc =
                    Ellipse2d.toEllipticalArc givenEllipse

                ellipticalCurve =
                    if Frame2d.isRightHanded (Ellipse2d.axes givenEllipse) then
                        Curve2d.ellipticalArc ellipticalArc

                    else
                        Curve2d.ellipticalArc (EllipticalArc2d.reverse ellipticalArc)
            in
            [ Curve2d.samples resolution ellipticalCurve ]

        Types.PolygonalRegion givenPolygon ->
            List.map (Curve2d.lineSegment >> Curve2d.samples resolution)
                (Polygon2d.edges givenPolygon)

        Types.BoundedRegion givenOuterLoop givenInnerLoops ->
            List.concat
                (loopSamples True resolution givenOuterLoop
                    :: List.map (loopSamples False resolution) givenInnerLoops
                )


addTriangle :
    Point2d units coordinates
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> Quantity Float (Squared units)
    -> Quantity Float (Squared units)
addTriangle p1 p2 p3 sumSoFar =
    sumSoFar |> Quantity.plus (Triangle2d.counterclockwiseArea (Triangle2d.from p1 p2 p3))


loopArea :
    List (List ( Point2d units coordinates, Direction2d coordinates ))
    -> Quantity Float (Squared units)
loopArea segments =
    case segments of
        [] ->
            Quantity.zero

        [] :: remainingSegments ->
            loopArea remainingSegments

        (( startPoint, _ ) :: _) :: remainingSegments ->
            Quantity.zero |> addSegments startPoint segments


addSegments :
    Point2d units coordinates
    -> List (List ( Point2d units coordinates, Direction2d coordinates ))
    -> Quantity Float (Squared units)
    -> Quantity Float (Squared units)
addSegments startPoint segments sumSoFar =
    case segments of
        [] ->
            sumSoFar

        [] :: remainingSegments ->
            addSegments startPoint remainingSegments sumSoFar

        [ singleSample ] :: remainingSegments ->
            addSegments startPoint remainingSegments sumSoFar

        (( firstPoint, _ ) :: ( secondPoint, _ ) :: remainingSamples) :: remainingSegments ->
            sumSoFar
                |> addTriangle startPoint firstPoint secondPoint
                |> addSamples startPoint secondPoint remainingSamples
                |> addSegments startPoint remainingSegments


addSamples :
    Point2d units coordinates
    -> Point2d units coordinates
    -> List ( Point2d units coordinates, Direction2d coordinates )
    -> Quantity Float (Squared units)
    -> Quantity Float (Squared units)
addSamples startPoint lastPoint samples sumSoFar =
    case samples of
        ( nextPoint, _ ) :: remainingSamples ->
            sumSoFar
                |> addTriangle startPoint lastPoint nextPoint
                |> addSamples startPoint nextPoint remainingSamples

        [] ->
            sumSoFar


loopSamples :
    Bool
    -> Quantity Float units
    -> List (Curve2d units coordinates)
    -> List (List ( Point2d units coordinates, Direction2d coordinates ))
loopSamples isOuter resolution curves =
    let
        joinedCurves =
            Curve2d.join True resolution curves

        initialSegments =
            List.map (Curve2d.samples resolution) joinedCurves
    in
    if isOuter == (loopArea initialSegments |> Quantity.greaterThanOrEqualTo Quantity.zero) then
        initialSegments

    else
        reverseSegments initialSegments []


reverseSegments :
    List (List ( Point2d units coordinates, Direction2d coordinates ))
    -> List (List ( Point2d units coordinates, Direction2d coordinates ))
    -> List (List ( Point2d units coordinates, Direction2d coordinates ))
reverseSegments segments accumulated =
    case segments of
        first :: rest ->
            reverseSegments rest (reverseSegment first [] :: accumulated)

        [] ->
            accumulated


reverseSegment :
    List ( Point2d units coordinates, Direction2d coordinates )
    -> List ( Point2d units coordinates, Direction2d coordinates )
    -> List ( Point2d units coordinates, Direction2d coordinates )
reverseSegment samples accumulated =
    case samples of
        ( firstPoint, firstDirection ) :: remainingSamples ->
            reverseSegment remainingSamples <|
                (( firstPoint, Direction2d.reverse firstDirection ) :: accumulated)

        [] ->
            accumulated


approximate :
    Quantity Float units
    -> Body3d units coordinates
    -> TriangularMesh { position : Point3d units coordinates, normal : Vector3d Unitless coordinates }
approximate resolution body =
    case body of
        Types.EmptyBody ->
            TriangularMesh.empty

        Types.RectangularBody givenBlock ->
            blockMesh givenBlock

        Types.SphericalBody givenSphere ->
            sphereMesh resolution givenSphere

        Types.CylindricalBody givenCylinder ->
            Debug.todo "TODO"

        Types.ConicalBody givenCone ->
            Debug.todo "TODO"

        Types.ExtrusionBody sketchPlane profile distance ->
            Debug.todo "TODO"

        Types.RevolutionBody sketchPlane profile axis2d angle ->
            Debug.todo "TODO"


blockMesh :
    Block3d units coordinates
    -> TriangularMesh { position : Point3d units coordinates, normal : Vector3d Unitless coordinates }
blockMesh givenBlock =
    let
        givenBlockAxes =
            Block3d.axes givenBlock

        rightHandedAxes =
            if Frame3d.isRightHanded givenBlockAxes then
                givenBlockAxes

            else
                Frame3d.reverseY givenBlockAxes

        rightHandedBlock =
            Block3d.centeredOn rightHandedAxes (Block3d.dimensions givenBlock)

        p0 =
            Block3d.interpolate rightHandedBlock 0 0 0

        p1 =
            Block3d.interpolate rightHandedBlock 0 0 1

        p2 =
            Block3d.interpolate rightHandedBlock 0 1 0

        p3 =
            Block3d.interpolate rightHandedBlock 0 1 1

        p4 =
            Block3d.interpolate rightHandedBlock 1 0 0

        p5 =
            Block3d.interpolate rightHandedBlock 1 0 1

        p6 =
            Block3d.interpolate rightHandedBlock 1 1 0

        p7 =
            Block3d.interpolate rightHandedBlock 1 1 1

        positiveX =
            Direction3d.toVector (Frame3d.xDirection rightHandedAxes)

        negativeX =
            Vector3d.reverse positiveX

        positiveY =
            Direction3d.toVector (Frame3d.yDirection rightHandedAxes)

        negativeY =
            Vector3d.reverse positiveY

        positiveZ =
            Direction3d.toVector (Frame3d.zDirection rightHandedAxes)

        negativeZ =
            Vector3d.reverse positiveZ

        v0x =
            { position = p0, normal = negativeX }

        v0y =
            { position = p0, normal = negativeY }

        v0z =
            { position = p0, normal = negativeZ }

        v1x =
            { position = p1, normal = negativeX }

        v1y =
            { position = p1, normal = negativeY }

        v1z =
            { position = p1, normal = positiveZ }

        v2x =
            { position = p2, normal = negativeX }

        v2y =
            { position = p2, normal = positiveY }

        v2z =
            { position = p2, normal = negativeZ }

        v3x =
            { position = p3, normal = negativeX }

        v3y =
            { position = p3, normal = positiveY }

        v3z =
            { position = p3, normal = positiveZ }

        v4x =
            { position = p4, normal = positiveX }

        v4y =
            { position = p4, normal = negativeY }

        v4z =
            { position = p4, normal = negativeZ }

        v5x =
            { position = p5, normal = positiveX }

        v5y =
            { position = p5, normal = negativeY }

        v5z =
            { position = p5, normal = positiveZ }

        v6x =
            { position = p6, normal = positiveX }

        v6y =
            { position = p6, normal = positiveY }

        v6z =
            { position = p6, normal = negativeZ }

        v7x =
            { position = p7, normal = positiveX }

        v7y =
            { position = p7, normal = positiveY }

        v7z =
            { position = p7, normal = positiveZ }

        vertices =
            Array.fromList
                [ v0x
                , v0y
                , v0z
                , v1x
                , v1y
                , v1z
                , v2x
                , v2y
                , v2z
                , v3x
                , v3y
                , v3z
                , v4x
                , v4y
                , v4z
                , v5x
                , v5y
                , v5z
                , v6x
                , v6y
                , v6z
                , v7x
                , v7y
                , v7z
                ]
    in
    TriangularMesh.indexed vertices blockFaceIndices


blockFaceIndices : List ( Int, Int, Int )
blockFaceIndices =
    [ ( 0, 3, 9 )
    , ( 0, 9, 6 )
    , ( 1, 13, 16 )
    , ( 1, 16, 4 )
    , ( 2, 8, 20 )
    , ( 2, 20, 14 )
    , ( 21, 15, 12 )
    , ( 21, 12, 18 )
    , ( 22, 19, 7 )
    , ( 22, 7, 10 )
    , ( 23, 11, 5 )
    , ( 23, 5, 17 )
    ]


sphereMesh :
    Quantity Float units
    -> Sphere3d units coordinates
    -> TriangularMesh { position : Point3d units coordinates, normal : Vector3d Unitless coordinates }
sphereMesh resolution givenSphere =
    let
        (Quantity r) =
            Sphere3d.radius givenSphere

        ( Quantity x0, Quantity y0, Quantity z0 ) =
            Point3d.coordinates (Sphere3d.centerPoint givenSphere)

        n =
            max 4 <|
                Curve.arcApproximationSegments
                    { maxError = resolution
                    , radius = Quantity r
                    , sweptAngle = Angle.radians (2 * pi)
                    }

        m =
            max 2 <|
                Curve.arcApproximationSegments
                    { maxError = resolution
                    , radius = Quantity r
                    , sweptAngle = Angle.radians pi
                    }

        sphereVertex u v =
            let
                theta =
                    2 * pi * u

                phi =
                    -pi / 2 + pi * v

                cosTheta =
                    cos theta

                sinTheta =
                    sin theta

                cosPhi =
                    cos phi

                sinPhi =
                    sin phi

                dx =
                    cosPhi * cosTheta

                dy =
                    cosPhi * sinTheta

                dz =
                    sinPhi
            in
            { position =
                Types.Point3d
                    { x = x0 + r * dx
                    , y = y0 + r * dy
                    , z = z0 + r * dz
                    }
            , normal =
                Types.Vector3d
                    { x = dx
                    , y = dy
                    , z = dz
                    }
            }
    in
    TriangularMesh.ball n m sphereVertex


cylinderMesh :
    Quantity Float units
    -> Cylinder3d units coordinates
    -> TriangularMesh { position : Point3d units coordinates, normal : Vector3d Unitless coordinates }
cylinderMesh resolution givenCylinder =
    let
        subdivisions =
            max 4 <|
                Curve.arcApproximationSegments
                    { maxError = resolution
                    , radius = Cylinder3d.radius givenCylinder
                    , sweptAngle = Angle.turns 1
                    }

        startPoint =
            Cylinder3d.startPoint givenCylinder

        axisDirection =
            Cylinder3d.axialDirection givenCylinder

        displacement =
            Vector3d.withLength (Cylinder3d.length givenCylinder) axisDirection

        forwardsNormal =
            Direction3d.toVector axisDirection

        backwardsNormal =
            Vector3d.reverse forwardsNormal

        startPoints =
            Cylinder3d.startCap givenCylinder
                |> Circle3d.toArc
                |> Arc3d.segments subdivisions
                |> Polyline3d.vertices

        startVertex point =
            { position = point
            , normal = backwardsNormal
            }

        endVertex point =
            { position = point |> Point3d.translateBy displacement
            , normal = forwardsNormal
            }

        startCapMesh =
            TriangularMesh.radial (startVertex startPoint) (List.map startVertex startPoints)

        endCapMesh =
            TriangularMesh.radial (endVertex startPoint) (List.reverse (List.map endVertex startPoints))
    in
    Debug.todo "TODO"
