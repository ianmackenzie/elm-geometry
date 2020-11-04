--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Circle3d exposing
    ( Circle3d
    , withRadius, sweptAround, on, throughPoints
    , centerPoint, axialDirection, radius, diameter, axis, plane, area, circumference, boundingBox
    , toArc
    , flip, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Circle3d` is defined by its center point, axial direction and radius. The
axial direction is the direction of the axis through the center of the circle
that all points on the circle are equidistant from, or equivalently the normal
direction of the plane defined by the circle. This module contains functionality
for:

  - Constructing circles around axes, on planes, or through points
  - Scaling, rotating and translating circles
  - Extracting circle properties like center point and area

@docs Circle3d


# Constructors

@docs withRadius, sweptAround, on, throughPoints


# Properties

@docs centerPoint, axialDirection, radius, diameter, axis, plane, area, circumference, boundingBox


# Conversion

@docs toArc


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs flip, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Arc3d exposing (Arc3d)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Ellipse2d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate, Squared)
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Circle3d units coordinates =
    Types.Circle3d units coordinates


{-| Construct a circle from its radius, axial direction and center point:

    exampleCircle =
        Circle3d.withRadius (Length.meters 3)
            Direction3d.z
            (Point3d.meters 2 0 1)

If you pass a negative radius, the absolute value will be used.

-}
withRadius : Quantity Float units -> Direction3d coordinates -> Point3d units coordinates -> Circle3d units coordinates
withRadius givenRadius givenAxialDirection givenCenterPoint =
    Types.Circle3d
        { centerPoint = givenCenterPoint
        , axialDirection = givenAxialDirection
        , radius = Quantity.abs givenRadius
        }


{-| Construct a circle by sweeping the given point around the given axis.

    Circle3d.sweptAround Axis3d.z (Point3d.meters 3 0 2)
    --> Circle3d.withRadius (Length.meters 3)
    -->     Direction3d.z
    -->     (Point3d.meters 0 0 2)

-}
sweptAround : Axis3d units coordinates -> Point3d units coordinates -> Circle3d units coordinates
sweptAround givenAxis givenPoint =
    let
        computedCenterPoint =
            givenPoint |> Point3d.projectOntoAxis givenAxis

        computedRadius =
            givenPoint |> Point3d.distanceFrom computedCenterPoint
    in
    withRadius computedRadius (Axis3d.direction givenAxis) computedCenterPoint


{-| Construct a 3D circle lying _on_ a sketch plane by providing a 2D circle
specified in XY coordinates _within_ the sketch plane.

    Circle3d.on SketchPlane3d.yz <|
        Circle2d.withRadius (Length.meters 3)
            (Point2d.meters 1 2)
    --> Circle3d.withRadius (Length.meters 3)
    -->     Direction3d.x
    -->     (Point3d.meters 0 1 2)

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Circle2d units coordinates2d -> Circle3d units coordinates3d
on sketchPlane circle =
    withRadius (Circle2d.radius circle)
        (SketchPlane3d.normalDirection sketchPlane)
        (Point3d.on sketchPlane (Circle2d.centerPoint circle))


{-| Attempt to construct a circle that passes through the three given points.
The axial direction of the returned circle will be such that the three points
are in counterclockwise order around it, according to the right-hand rule. If
the three given points are collinear, returns `Nothing`.

    Circle3d.throughPoints
        (Point3d.meters 1 0 0)
        (Point3d.meters 0 1 0)
        (Point3d.meters 0 0 1)
    --> Just <|
    -->     Circle3d.withRadius (Length.meters 0.8165)
    -->         (Direction3d.xyZ
    -->             (Angle.degrees 45)
    -->             (Angle.degrees 35.26)
    -->         )
    -->         (Point3d.meters 0.333 0.333 0.333)

-}
throughPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Circle3d units coordinates)
throughPoints p1 p2 p3 =
    Maybe.map2
        (\computedCenterPoint computedPlane ->
            let
                r1 =
                    Point3d.distanceFrom computedCenterPoint p1

                r2 =
                    Point3d.distanceFrom computedCenterPoint p2

                r3 =
                    Point3d.distanceFrom computedCenterPoint p3

                computedRadius =
                    Quantity.multiplyBy (1 / 3)
                        (r1 |> Quantity.plus r2 |> Quantity.plus r3)
            in
            withRadius computedRadius
                (Plane3d.normalDirection computedPlane)
                computedCenterPoint
        )
        (Point3d.circumcenter p1 p2 p3)
        (Plane3d.throughPoints p1 p2 p3)


{-| Convert a circle from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Circle3d units1 coordinates -> Circle3d units2 coordinates
at rate (Types.Circle3d circle) =
    Types.Circle3d
        { centerPoint = Point3d.at rate circle.centerPoint
        , axialDirection = circle.axialDirection
        , radius = Quantity.abs (Quantity.at rate circle.radius)
        }


{-| Convert a circle from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Circle3d units1 coordinates -> Circle3d units2 coordinates
at_ rate circle =
    at (Quantity.inverse rate) circle


{-| Get the center point of a circle.
-}
centerPoint : Circle3d units coordinates -> Point3d units coordinates
centerPoint (Types.Circle3d circle) =
    circle.centerPoint


{-| Get the axial direction of a circle.
-}
axialDirection : Circle3d units coordinates -> Direction3d coordinates
axialDirection (Types.Circle3d circle) =
    circle.axialDirection


{-| Get the central axis of a circle, perpendicular to its [`plane`](#plane).
The origin point of the returned axis will be the center point of the circle.
-}
axis : Circle3d units coordinates -> Axis3d units coordinates
axis (Types.Circle3d circle) =
    Axis3d.through circle.centerPoint circle.axialDirection


{-| Get the plane that a circle lies in. The origin point of the returned plane
will be the center point of the circle, and its normal direction will be the
axial direction of the circle.
-}
plane : Circle3d units coordinates -> Plane3d units coordinates
plane circle =
    Plane3d.through (centerPoint circle) (axialDirection circle)


{-| Get the radius of a circle.
-}
radius : Circle3d units coordinates -> Quantity Float units
radius (Types.Circle3d properties) =
    properties.radius


{-| Get the diameter of a circle (twice its radius).
-}
diameter : Circle3d units coordinates -> Quantity Float units
diameter circle =
    Quantity.multiplyBy 2 (radius circle)


{-| Get the area of a circle.
-}
area : Circle3d units coordinates -> Quantity Float (Squared units)
area circle =
    Quantity.multiplyBy pi (Quantity.squared (radius circle))


{-| Get the circumference (perimeter) of a circle.
-}
circumference : Circle3d units coordinates -> Quantity Float units
circumference circle =
    Quantity.multiplyBy (2 * pi) (radius circle)


{-| Flip the axial direction of a circle.
-}
flip : Circle3d units coordinates -> Circle3d units coordinates
flip circle =
    withRadius (radius circle)
        (Direction3d.reverse (axialDirection circle))
        (centerPoint circle)


{-| Scale a circle around a given point by a given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Circle3d units coordinates -> Circle3d units coordinates
scaleAbout point scale circle =
    withRadius (Quantity.multiplyBy (abs scale) (radius circle))
        (if scale >= 0 then
            axialDirection circle

         else
            Direction3d.reverse (axialDirection circle)
        )
        (Point3d.scaleAbout point scale (centerPoint circle))


{-| Rotate a circle around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Circle3d units coordinates -> Circle3d units coordinates
rotateAround givenAxis givenAngle circle =
    withRadius (radius circle)
        (Direction3d.rotateAround givenAxis givenAngle (axialDirection circle))
        (Point3d.rotateAround givenAxis givenAngle (centerPoint circle))


{-| Translate a circle by a given displacement.
-}
translateBy : Vector3d units coordinates -> Circle3d units coordinates -> Circle3d units coordinates
translateBy displacement circle =
    withRadius (radius circle)
        (axialDirection circle)
        (Point3d.translateBy displacement (centerPoint circle))


{-| Translate a circle in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Circle3d units coordinates -> Circle3d units coordinates
translateIn direction distance circle =
    translateBy (Vector3d.withLength distance direction) circle


{-| Mirror a circle across a given plane.
-}
mirrorAcross : Plane3d units coordinates -> Circle3d units coordinates -> Circle3d units coordinates
mirrorAcross mirrorPlane circle =
    withRadius (radius circle)
        (Direction3d.mirrorAcross mirrorPlane (axialDirection circle))
        (Point3d.mirrorAcross mirrorPlane (centerPoint circle))


{-| Project a circle into a sketch plane. Note that the result is in general an
ellipse, not a circle!

    inclinedCircle : Circle3d
    inclinedCircle =
        Circle3d.withRadius (Length.meters 1)
            (Direction3d.xz (Angle.degrees 45))
            (Point3d.meters 1 2 3)

    Circle3d.projectInto SketchPlane3d.xy inclinedCircle
    --> Ellipse2d.with
    -->     { centerPoint = Point2d.meters 1 2
    -->     , xDirection = Direction2d.negativeY
    -->     , xRadius = Length.meters 1
    -->     , yRadius = Length.meters 0.7071
    -->     }

The X radius of the returned ellipse will always be greater than or equal to the
Y radius (the X axis will be the major axis and the Y axis will be the minor
axis). Note that if the 3D circle is perfectly parallel to the sketch plane,
then the resulting ellipse will be circular (its X and Y radii will be equal)
and its X and Y axis directions will be chosen arbitrarily.

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Circle3d units coordinates3d -> Ellipse2d units coordinates2d
projectInto sketchPlane circle =
    let
        projectedAxialDirection =
            axialDirection circle |> Direction3d.projectInto sketchPlane

        projectedCenter =
            centerPoint circle |> Point3d.projectInto sketchPlane

        xRadius =
            radius circle
    in
    case projectedAxialDirection of
        Just yDirection ->
            let
                normalDirection =
                    SketchPlane3d.normalDirection sketchPlane

                yRatio =
                    axialDirection circle
                        |> Direction3d.componentIn normalDirection
                        |> abs

                yRadius =
                    Quantity.multiplyBy yRatio xRadius

                axes =
                    Frame2d.withYDirection yDirection projectedCenter
            in
            Types.Ellipse2d
                { axes = axes
                , xRadius = xRadius
                , yRadius = yRadius
                }

        Nothing ->
            Types.Ellipse2d
                { axes = Frame2d.atPoint projectedCenter
                , xRadius = xRadius
                , yRadius = xRadius
                }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Circle3d units globalCoordinates -> Circle3d units localCoordinates
relativeTo frame circle =
    withRadius (radius circle)
        (Direction3d.relativeTo frame (axialDirection circle))
        (Point3d.relativeTo frame (centerPoint circle))


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Circle3d units localCoordinates -> Circle3d units globalCoordinates
placeIn frame circle =
    withRadius (radius circle)
        (Direction3d.placeIn frame (axialDirection circle))
        (Point3d.placeIn frame (centerPoint circle))


{-| Get the minimal bounding box containing a given circle.

    Circle3d.boundingBox exampleCircle
    --> BoundingBox3d.from
    -->     (Point3d.meters -1 -3 1)
    -->     (Point3d.meters 5 3 1)

-}
boundingBox : Circle3d units coordinates -> BoundingBox3d units coordinates
boundingBox circle =
    let
        axisDirection =
            axialDirection circle

        nx =
            Direction3d.xComponent axisDirection

        ny =
            Direction3d.yComponent axisDirection

        nz =
            Direction3d.zComponent axisDirection

        nx2 =
            nx * nx

        ny2 =
            ny * ny

        nz2 =
            nz * nz

        r =
            radius circle

        dx =
            r |> Quantity.multiplyBy (sqrt (ny2 + nz2))

        dy =
            r |> Quantity.multiplyBy (sqrt (nx2 + nz2))

        dz =
            r |> Quantity.multiplyBy (sqrt (nx2 + ny2))

        p0 =
            centerPoint circle
    in
    BoundingBox3d.fromExtrema
        { minX = Point3d.xCoordinate p0 |> Quantity.minus dx
        , maxX = Point3d.xCoordinate p0 |> Quantity.plus dx
        , minY = Point3d.yCoordinate p0 |> Quantity.minus dy
        , maxY = Point3d.yCoordinate p0 |> Quantity.plus dy
        , minZ = Point3d.zCoordinate p0 |> Quantity.minus dz
        , maxZ = Point3d.zCoordinate p0 |> Quantity.plus dz
        }


{-| Convert a circle to a 360 degree arc. The start point of the arc is
unspecified.
-}
toArc : Circle3d units coordinates -> Arc3d units coordinates
toArc circle =
    let
        -- Find an arbitrary radial direction perpendicular to the circle axis
        radialDirection =
            Direction3d.perpendicularTo (axialDirection circle)

        -- Move the center point outwards by the circle radius to get a point on
        -- the circle
        startPoint =
            centerPoint circle
                |> Point3d.translateIn radialDirection (radius circle)
    in
    Arc3d.sweptAround (axis circle) (Angle.turns 1) startPoint
