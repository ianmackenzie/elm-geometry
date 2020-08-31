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
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Circle2d exposing (Circle2d)
import Cone3d exposing (Cone3d)
import Curve2d exposing (Curve2d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity, Squared)
import Quantity.Interval as Interval exposing (Interval)
import Rectangle2d exposing (Rectangle2d)
import Region2d exposing (Region2d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
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
