module Circle3d
    exposing
        ( Circle3d
        , area
        , axialDirection
        , axis
        , boundingBox
        , centerPoint
        , circumference
        , diameter
        , mirrorAcross
        , on
        , placeIn
        , plane
        , projectInto
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , sweptAround
        , throughPoints
        , translateBy
        , withRadius
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle3d.svg" alt="Circle3d" width="160">

A `Circle3d` is defined by its center point, axial direction and radius. The
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

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Internal as Internal
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Circle3d =
    Internal.Circle3d


{-| Construct a circle from its radius, axial direction and center point:

    exampleCircle =
        Circle3d.withRadius 3
            Direction3d.z
            (Point3d.fromCoordinates ( 2, 0, 1 ))

The actual radius of the circle will be the absolute value of the given radius
(passing `-3` will have the same effect as `3`).

-}
withRadius : Float -> Direction3d -> Point3d -> Circle3d
withRadius radius axialDirection centerPoint =
    Internal.Circle3d
        { centerPoint = centerPoint
        , axialDirection = axialDirection
        , radius = abs radius
        }


{-| Construct a circle by sweeping the given point around the given axis.

    Circle3d.sweptAround Axis3d.z
        (Point3d.fromCoordinates ( 3, 0, 2 ))
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 0, 0, 2 ))

-}
sweptAround : Axis3d -> Point3d -> Circle3d
sweptAround axis point =
    let
        centerPoint =
            Point3d.projectOntoAxis axis point
    in
    withRadius (Point3d.distanceFrom centerPoint point)
        (Axis3d.direction axis)
        centerPoint


{-| Construct a 3D circle lying _on_ a sketch plane by providing a 2D circle
specified in XY coordinates _within_ the sketch plane.

    Circle3d.on SketchPlane3d.yz <|
        Circle2d.withRadius 3
            (Point2d.fromCoordinates ( 1, 2 ))

    --> Circle3d.withRadius 3
    -->     Direction3d.x
    -->     (Point3d.fromCoordinates ( 0, 1, 2 ))

-}
on : SketchPlane3d -> Circle2d -> Circle3d
on sketchPlane circle =
    withRadius (Circle2d.radius circle)
        (SketchPlane3d.normalDirection sketchPlane)
        (Point3d.on sketchPlane (Circle2d.centerPoint circle))


{-| Attempt to construct a circle that passes through the three given points.
The axial direction of the returned circle will be such that the three points
are in counterclockwise order around it, according to the right-hand rule. If
the three given points are collinear, returns `Nothing`.

    Circle3d.throughPoints
        ( Point3d.fromCoordinates ( 1, 0, 0 )
        , Point3d.fromCoordinates ( 0, 1, 0 )
        , Point3d.fromCoordinates ( 0, 0, 1 )
        )
    --> Just
    -->     (Circle3d.withRadius 0.8165
    -->         (Direction3d.fromAzimuthAndElevation
    -->             ( degrees 45, degrees 35.26 )
    -->         )
    -->         (Point3d.fromCoordinates
    -->             ( 0.333, 0.333, 0.333 )
    -->         )
    -->     )

-}
throughPoints : ( Point3d, Point3d, Point3d ) -> Maybe Circle3d
throughPoints points =
    Maybe.map2
        (\centerPoint plane ->
            let
                ( p1, p2, p3 ) =
                    points

                r1 =
                    Point3d.distanceFrom centerPoint p1

                r2 =
                    Point3d.distanceFrom centerPoint p2

                r3 =
                    Point3d.distanceFrom centerPoint p3

                r =
                    (r1 + r2 + r3) / 3
            in
            withRadius r (Plane3d.normalDirection plane) centerPoint
        )
        (Point3d.circumcenter points)
        (Plane3d.throughPoints points)


{-| Get the center point of a circle.

    Circle3d.centerPoint exampleCircle
    --> Point3d.fromCoordinates ( 2, 0, 1 )

-}
centerPoint : Circle3d -> Point3d
centerPoint (Internal.Circle3d circle) =
    circle.centerPoint


{-| Get the axial direction of a circle.

    Circle3d.axialDirection exampleCircle
    --> Direction3d.z

-}
axialDirection : Circle3d -> Direction3d
axialDirection (Internal.Circle3d circle) =
    circle.axialDirection


{-| Get the central axis of a circle, perpendicular to its [`plane`](#plane).
The origin point of the returned axis will be the center point of the circle.

    Circle3d.axis exampleCircle
    --> Axis3d.withDirection Direction3d.z
    -->     (Point3d.fromCoordinates ( 2, 0, 1 ))

-}
axis : Circle3d -> Axis3d
axis (Internal.Circle3d circle) =
    Axis3d.withDirection circle.axialDirection circle.centerPoint


{-| Get the plane that a circle lies in. The origin point of the returned plane
will be the center point of the circle, and its normal direction will be the
axial direction of the circle.

    Circle3d.plane exampleCircle
    --> Plane3d.with
    -->     { originPoint =
    -->         Point3d.fromCoordinates ( 2, 0, 1 )
    -->     , normalDirection = Direction3d.z
    -->     }

-}
plane : Circle3d -> Plane3d
plane circle =
    Plane3d.with
        { originPoint = centerPoint circle
        , normalDirection = axialDirection circle
        }


{-| Get the radius of a circle.

    Circle3d.radius exampleCircle
    --> 3

-}
radius : Circle3d -> Float
radius (Internal.Circle3d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circl3d.diameter exampleCircle
    --> 6

-}
diameter : Circle3d -> Float
diameter circle =
    2 * radius circle


{-| Get the area of a circle.

    Circle3d.area exampleCircle
    --> 28.2743

-}
area : Circle3d -> Float
area circle =
    let
        r =
            radius circle
    in
    pi * r * r


{-| Get the circumference of a circle.

    Circle3d.circumference exampleCircle
    --> 18.8496

-}
circumference : Circle3d -> Float
circumference circle =
    2 * pi * radius circle


{-| Scale a circle around a given point by a given scale.

    Circle3d.scaleAbout Point3d.origin 3 exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 6, 0, 3 ))

-}
scaleAbout : Point3d -> Float -> Circle3d -> Circle3d
scaleAbout point scale circle =
    withRadius (abs scale * radius circle)
        (if scale >= 0 then
            axialDirection circle
         else
            Direction3d.flip (axialDirection circle)
        )
        (Point3d.scaleAbout point scale (centerPoint circle))


{-| Rotate a circle around a given axis by a given angle (in radians).

    exampleCircle
        |> Circle3d.rotateAround Axis3d.y (degrees 90)
    --> Circle3d.withRadius 3
    -->     Direction3d.x
    -->     (Point3d.fromCoordinates ( 1, 0, -2 ))

-}
rotateAround : Axis3d -> Float -> Circle3d -> Circle3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
    \circle ->
        withRadius (radius circle)
            (rotateDirection (axialDirection circle))
            (rotatePoint (centerPoint circle))


{-| Translate a circle by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 1, 3 )

    Circle3d.translateBy displacement exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 4, 1, 4 ))

-}
translateBy : Vector3d -> Circle3d -> Circle3d
translateBy displacement circle =
    withRadius (radius circle)
        (axialDirection circle)
        (Point3d.translateBy displacement (centerPoint circle))


{-| Mirror a circle across a given plane.

    Circle3d.mirrorAcross Plane3d.xy exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.negativeZ
    -->     (Point3d.fromCoordinates ( 2, 0, -1 ))

-}
mirrorAcross : Plane3d -> Circle3d -> Circle3d
mirrorAcross plane circle =
    withRadius (radius circle)
        (Direction3d.mirrorAcross plane (axialDirection circle))
        (Point3d.mirrorAcross plane (centerPoint circle))


{-| Project a circle into a sketch plane.

    inclinedCircle : Circle3d
    inclinedCircle =
        Circle3d.withRadius 1
            (Direction3d.fromAzimuthAndElevation
                ( 0, degrees 45 )
            )
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Circle3d.projectInto SketchPlane3d.xy inclinedCircle
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , xDirection = Direction2d.negativeY
    -->     , xRadius = 1
    -->     , yRadius = 0.7071
    -->     }

-}
projectInto : SketchPlane3d -> Circle3d -> Internal.Ellipse2d
projectInto sketchPlane circle =
    let
        projectedAxialDirection =
            axialDirection circle
                |> Direction3d.toVector
                |> Vector3d.projectInto sketchPlane

        projectedCenter =
            centerPoint circle |> Point3d.projectInto sketchPlane

        xRadius =
            radius circle
    in
    case Vector2d.direction projectedAxialDirection of
        Just yDirection ->
            let
                xDirection =
                    yDirection |> Direction2d.rotateClockwise

                normalDirection =
                    SketchPlane3d.normalDirection sketchPlane

                yRatio =
                    axialDirection circle
                        |> Direction3d.componentIn normalDirection
                        |> abs

                yRadius =
                    yRatio * xRadius

                axes =
                    Frame2d.with
                        { originPoint = projectedCenter
                        , xDirection = xDirection
                        }
            in
            Internal.Ellipse2d
                { axes = axes
                , xRadius = xRadius
                , yRadius = yRadius
                }

        Nothing ->
            Internal.Ellipse2d
                { axes = Frame2d.atPoint projectedCenter
                , xRadius = xRadius
                , yRadius = xRadius
                }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Circle3d.relativeTo localFrame exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 1, -2, -2 ))

-}
relativeTo : Frame3d -> Circle3d -> Circle3d
relativeTo frame circle =
    withRadius (radius circle)
        (Direction3d.relativeTo frame (axialDirection circle))
        (Point3d.relativeTo frame (centerPoint circle))


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Circle3d.placeIn localFrame exampleCircle
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 3, 2, 4 ))

-}
placeIn : Frame3d -> Circle3d -> Circle3d
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
boundingBox : Circle3d -> BoundingBox3d
boundingBox circle =
    let
        ( nx, ny, nz ) =
            Direction3d.components (axialDirection circle)

        nx2 =
            nx * nx

        ny2 =
            ny * ny

        nz2 =
            nz * nz

        r =
            radius circle

        dx =
            r * sqrt (ny2 + nz2)

        dy =
            r * sqrt (nx2 + nz2)

        dz =
            r * sqrt (nx2 + ny2)

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint circle)
    in
    BoundingBox3d.fromExtrema
        { minX = cx - dx
        , maxX = cx + dx
        , minY = cy - dy
        , maxY = cy + dy
        , minZ = cz - dz
        , maxZ = cz + dz
        }
