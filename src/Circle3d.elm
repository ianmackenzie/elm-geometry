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
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto
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


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Ellipse2d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Squared)
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Circle3d units coordinates =
    Types.Circle3d units coordinates


{-| Construct a circle from its radius, axial direction and center point:

    exampleCircle =
        Circle3d.withRadius 3
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

    Circle3d.sweptAround Axis3d.z
        (Point3d.meters 3 0 2)
    --> Circle3d.withRadius 3
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
        Circle2d.withRadius 3
            (Point2d.meters 1 2)

    --> Circle3d.withRadius 3
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
    --> Just
    -->     (Circle3d.withRadius 0.8165
    -->         (Direction3d.fromAzimuthAndElevation
    -->             (Angle.degrees 45)
    -->             (Angle.degrees 35.26)
    -->         )
    -->         (Point3d.fromCoordinates
    -->             ( 0.333, 0.333, 0.333 )
    -->         )
    -->     )

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


{-| Get the center point of a circle.

    Circle3d.centerPoint exampleCircle
    --> Point3d.meters 2 0 1

-}
centerPoint : Circle3d units coordinates -> Point3d units coordinates
centerPoint (Types.Circle3d circle) =
    circle.centerPoint


{-| Get the axial direction of a circle.

    Circle3d.axialDirection exampleCircle
    --> Direction3d.z

-}
axialDirection : Circle3d units coordinates -> Direction3d coordinates
axialDirection (Types.Circle3d circle) =
    circle.axialDirection


{-| Get the central axis of a circle, perpendicular to its [`plane`](#plane).
The origin point of the returned axis will be the center point of the circle.

    Circle3d.axis exampleCircle
    --> Axis3d.withDirection Direction3d.z
    -->     (Point3d.meters 2 0 1)

-}
axis : Circle3d units coordinates -> Axis3d units coordinates
axis (Types.Circle3d circle) =
    Axis3d.through circle.centerPoint circle.axialDirection


{-| Get the plane that a circle lies in. The origin point of the returned plane
will be the center point of the circle, and its normal direction will be the
axial direction of the circle.

    Circle3d.plane exampleCircle
    --> Plane3d.withNormalDirection Direction3d.z
    -->     (Point3d.meters 2 0 1)

-}
plane : Circle3d units coordinates -> Plane3d units coordinates
plane circle =
    Plane3d.through (centerPoint circle) (axialDirection circle)


{-| Get the radius of a circle.

    Circle3d.radius exampleCircle
    --> 3

-}
radius : Circle3d units coordinates -> Quantity Float units
radius (Types.Circle3d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circl3d.diameter exampleCircle
    --> 6

-}
diameter : Circle3d units coordinates -> Quantity Float units
diameter circle =
    Quantity.multiplyBy 2 (radius circle)


{-| Get the area of a circle.

    Circle3d.area exampleCircle
    --> 28.2743

-}
area : Circle3d units coordinates -> Quantity Float (Squared units)
area circle =
    Quantity.multiplyBy pi (Quantity.squared (radius circle))


{-| Get the circumference of a circle.

    Circle3d.circumference exampleCircle
    --> 18.8496

-}
circumference : Circle3d units coordinates -> Quantity Float units
circumference circle =
    Quantity.multiplyBy (2 * pi) (radius circle)


{-| Scale a circle around a given point by a given scale.

    Circle3d.scaleAbout Point3d.origin 3 exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.meters 6 0 3)

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


{-| Rotate a circle around a given axis by a given angle (in radians).

    exampleCircle
        |> Circle3d.rotateAround Axis3d.y (Angle.degrees 90)
    --> Circle3d.withRadius 3
    -->     Direction3d.x
    -->     (Point3d.meters 1 0 -2)

-}
rotateAround : Axis3d units coordinates -> Angle -> Circle3d units coordinates -> Circle3d units coordinates
rotateAround givenAxis givenAngle circle =
    withRadius (radius circle)
        (Direction3d.rotateAround givenAxis givenAngle (axialDirection circle))
        (Point3d.rotateAround givenAxis givenAngle (centerPoint circle))


{-| Translate a circle by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 1, 3 )

    Circle3d.translateBy displacement exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.meters 4 1 4)

-}
translateBy : Vector3d units coordinates -> Circle3d units coordinates -> Circle3d units coordinates
translateBy displacement circle =
    withRadius (radius circle)
        (axialDirection circle)
        (Point3d.translateBy displacement (centerPoint circle))


{-| Translate a circle in a given direction by a given distance;

    Circle3d.translateIn direction distance

is equivalent to

    Circle3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Circle3d units coordinates -> Circle3d units coordinates
translateIn direction distance circle =
    translateBy (Vector3d.withLength distance direction) circle


{-| Mirror a circle across a given plane.

    Circle3d.mirrorAcross Plane3d.xy exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.negativeZ
    -->     (Point3d.meters 2 0 -1)

-}
mirrorAcross : Plane3d units coordinates -> Circle3d units coordinates -> Circle3d units coordinates
mirrorAcross mirrorPlane circle =
    withRadius (radius circle)
        (Direction3d.mirrorAcross mirrorPlane (axialDirection circle))
        (Point3d.mirrorAcross mirrorPlane (centerPoint circle))


{-| Project a circle into a sketch plane.

    inclinedCircle : Circle3d
    inclinedCircle =
        Circle3d.withRadius 1
            (Direction3d.fromAzimuthAndElevation
                (Angle.degrees 0)
                (Angle.degrees 45)
            )
            (Point3d.meters 1 2 3)

    Circle3d.projectInto SketchPlane3d.xy inclinedCircle
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.meters 1 2
    -->     , xDirection = Direction2d.negativeY
    -->     , xRadius = 1
    -->     , yRadius = 0.7071
    -->     }

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

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    Circle3d.relativeTo localFrame exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.meters 1 -2 -2)

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Circle3d units globalCoordinates -> Circle3d units localCoordinates
relativeTo frame circle =
    withRadius (radius circle)
        (Direction3d.relativeTo frame (axialDirection circle))
        (Point3d.relativeTo frame (centerPoint circle))


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    Circle3d.placeIn localFrame exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.meters 3 2 4)

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Circle3d units localCoordinates -> Circle3d units globalCoordinates
placeIn frame circle =
    withRadius (radius circle)
        (Direction3d.placeIn frame (axialDirection circle))
        (Point3d.placeIn frame (centerPoint circle))


{-| Get the minimal bounding box containing a given circle.

    Circle3d.boundingBox exampleCircle
    --> BoundingBox3d.fromExtrema
    -->     { minX = -1
    -->     , maxX = 5
    -->     , minY = -3
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 1
    -->     }

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
