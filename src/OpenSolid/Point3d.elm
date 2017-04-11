--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.Point3d
    exposing
        ( origin
        , midpoint
        , interpolateFrom
        , interpolate
        , along
        , on
        , in_
        , coordinates
        , xCoordinate
        , yCoordinate
        , zCoordinate
        , equalWithin
        , vectorFrom
        , directionFrom
        , distanceFrom
        , squaredDistanceFrom
        , distanceAlong
        , radialDistanceFrom
        , squaredRadialDistanceFrom
        , signedDistanceFrom
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , projectRadiallyOnto
        , relativeTo
        , placeIn
        , projectInto
        , hull
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/point3d.svg" alt="Point3d" width="160">

A `Point3d` represents a position in 3D space and is defined by its X, Y and Z
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points, or the distance of a point from an axis
    or a plane
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

The simplest way to construct a `Point3d` value is by passing a tuple of X, Y
and Z coordinates to the `Point3d` constructor, for example

    point =
        Point3d ( 2, 1, 3 )


# Predefined points

@docs origin


# Constructors

@docs midpoint, interpolateFrom, interpolate, along, on, in_


# Coordinates

@docs coordinates, xCoordinate, yCoordinate, zCoordinate


# Comparison

@docs equalWithin


# Displacement and distance

@docs vectorFrom, directionFrom, distanceFrom, squaredDistanceFrom, distanceAlong, radialDistanceFrom, squaredRadialDistanceFrom, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, projectRadiallyOnto


# Coordinate frames

Functions for transforming points between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Bounds

@docs hull

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Scalar as Scalar
import OpenSolid.Bootstrap.Axis3d as Axis3d
import OpenSolid.Bootstrap.Plane3d as Plane3d
import OpenSolid.Bootstrap.SketchPlane3d as SketchPlane3d
import OpenSolid.Bootstrap.Frame3d as Frame3d


addTo : Point3d -> Vector3d -> Point3d
addTo =
    flip translateBy


{-| The point (0, 0, 0).

    Point3d.origin
    --> Point3d ( 0, 0, 0 )

-}
origin : Point3d
origin =
    Point3d ( 0, 0, 0 )


{-| Construct a point halfway between two other points.

    p1 =
        Point3d ( 1, 1, 1 )

    p2 =
        Point3d ( 3, 7, 9 )

    Point3d.midpoint p1 p2
    --> Point3d ( 2, 4, 5 )

-}
midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point3d ( 1, 2, 4 )

    endPoint =
        Point3d ( 1, 2, 8 )

    Point3d.interpolateFrom startPoint endPoint 0.25
    --> Point3d ( 1, 2, 5 )

Partial application may be useful:

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point3d ( 1, 2, 4 )
    --> , Point3d ( 1, 2, 6 )
    --> , Point3d ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point3d ( 1, 2, 2 )

    interpolatedPoint 1.25
    --> Point3d ( 1, 2, 9 )

-}
interpolateFrom : Point3d -> Point3d -> Float -> Point3d
interpolateFrom p1 p2 t =
    let
        ( x1, y1, z1 ) =
            coordinates p1

        ( x2, y2, z2 ) =
            coordinates p2
    in
        Point3d
            ( Scalar.interpolateFrom x1 x2 t
            , Scalar.interpolateFrom y1 y2 t
            , Scalar.interpolateFrom z1 z2 t
            )


{-| DEPRECATED: Alias for `interpolateFrom`, kept for compatibility. Use
`interpolateFrom` instead.
-}
interpolate : Point3d -> Point3d -> Float -> Point3d
interpolate =
    interpolateFrom


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point3d.along Axis3d.z 2
    --> Point3d ( 0, 0, 2 )

Positive and negative distances are interpreted relative to the direction of the
axis:

    horizontalAxis =
        Axis3d
            { originPoint = Point3d ( 1, 1, 1 )
            , direction = Direction3d ( -1, 0, 0 )
            }

    Point3d.along horizontalAxis 3
    --> Point3d ( -2, 1, 1 )

    Point3d.along horizontalAxis -3
    --> Point3d ( 4, 1, 1 )

-}
along : Axis3d -> Float -> Point3d
along axis distance =
    Axis3d.originPoint axis
        |> translateBy (Vector3d.in_ (Axis3d.direction axis) distance)


{-| Construct a point on a sketch plane with the given local coordinates.

    Point3d.on SketchPlane3d.xz ( 2, 3 )
    --> Point3d ( 2, 0, 3 )

This is shorthand for using `Point2d.placeOnto`;

    Point3d.on sketchPlane coordinates

is equivalent to

    Point2d coordinates |> Point2d.placeOnto sketchPlane

-}
on : SketchPlane3d -> ( Float, Float ) -> Point3d
on sketchPlane coordinates =
    Point2d.placeOnto sketchPlane (Point2d coordinates)


{-| Construct a point given its local coordinates within a particular frame.

    frame =
        Frame3d.at (Point3d ( 1, 1, 1 ))

    Point3d.in_ frame ( 1, 2, 3 )
    --> Point3d ( 2, 3, 4 )

This is shorthand for using `Point3d.placeIn`;

    Point3d.in_ frame coordinates

is equivalent to

    Point3d coordinates |> Point3d.placeIn frame

-}
in_ : Frame3d -> ( Float, Float, Float ) -> Point3d
in_ frame coordinates =
    placeIn frame (Point3d coordinates)


{-| Get the coordinates of a point as a tuple.

    ( x, y, z ) =
        Point3d.coordinates point

-}
coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_


{-| Get the X coordinate of a point.

    Point3d.xCoordinate (Point3d ( 2, 1, 3 ))
    --> 2

-}
xCoordinate : Point3d -> Float
xCoordinate (Point3d ( x, _, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point3d.yCoordinate (Point3d ( 2, 1, 3 ))
    --> 1

-}
yCoordinate : Point3d -> Float
yCoordinate (Point3d ( _, y, _ )) =
    y


{-| Get the Z coordinate of a point.

    Point3d.zCoordinate (Point3d ( 2, 1, 3 ))
    --> 3

-}
zCoordinate : Point3d -> Float
zCoordinate (Point3d ( _, _, z )) =
    z


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point3d ( 2, 1, 3 )

    secondPoint =
        Point3d ( 2.0002, 0.9999, 3.0001 )

    Point3d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point3d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Float -> Point3d -> Point3d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| Find the vector from one point to another.

    startPoint =
        Point3d ( 1, 1, 1 )

    endPoint =
        Point3d ( 4, 5, 6 )

    Point3d.vectorFrom startPoint endPoint
    --> Vector3d ( 3, 4, 5 )

-}
vectorFrom : Point3d -> Point3d -> Vector3d
vectorFrom firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            coordinates firstPoint

        ( x2, y2, z2 ) =
            coordinates secondPoint
    in
        Vector3d ( x2 - x1, y2 - y1, z2 - z1 )


{-| Attempt to find the direction from the first point to the second. If the two
points are coincident, returns `Nothing`.

    point =
        Point3d ( 1, 0, 1 )

    Point3d.directionFrom Point3d.origin point
    --> Just (Direction3d ( 0.7071, 0, 0.7071 ))

    Point3d.directionFrom point Point3d.origin
    --> Just (Direction3d ( -0.7071, 0, -0.7071 ))

    Point3d.directionFrom point point
    --> Nothing

-}
directionFrom : Point3d -> Point3d -> Maybe Direction3d
directionFrom firstPoint secondPoint =
    Vector3d.direction (vectorFrom firstPoint secondPoint)


{-| Find the distance between two points.

    p1 =
        Point3d ( 1, 1, 2 )

    p2 =
        Point3d ( 2, 3, 4 )

    Point3d.distanceFrom p1 p2
    --> 3

Partial application can be useful:

    points =
        [ Point3d ( 3, 4, 5 )
        , Point3d ( 10, 10, 10 )
        , Point3d ( -1, 2, -3 )
        ]

    distanceFromOrigin : Point3d -> Float
    distanceFromOrigin =
        Point3d.distanceFrom Point3d.origin

    List.sortBy distanceFromOrigin points
    --> [ Point3d ( -1, 2, -3 )
    --> , Point3d ( 3, 4, 5 )
    --> , Point3d ( 10, 10, 10 )
    --> ]

-}
distanceFrom : Point3d -> Point3d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point3d.squaredDistanceFrom p1 p2 > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point3d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector3d.squaredLength (vectorFrom firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d
            { originPoint = Point3d ( 1, 0, 0 )
            , direction = Direction3d.x
            }

    point =
        Point3d ( 3, 3, 3 )

    Point3d.distanceAlong axis point
    --> 2

    Point3d.distanceAlong axis Point3d.origin
    --> -1

-}
distanceAlong : Axis3d -> Point3d -> Float
distanceAlong axis point =
    vectorFrom (Axis3d.originPoint axis) point
        |> Vector3d.componentIn (Axis3d.direction axis)


{-| Find the perpendicular (nearest) distance of a point from an axis.

    point =
        Point3d ( -3, 4, 0 )

    Point3d.radialDistanceFrom Axis3d.x point
    --> 4

    Point3d.radialDistanceFrom Axis3d.y point
    --> 3

    Point3d.radialDistanceFrom Axis3d.z point
    --> 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.

-}
radialDistanceFrom : Axis3d -> Point3d -> Float
radialDistanceFrom axis point =
    sqrt (squaredRadialDistanceFrom axis point)


{-| Find the square of the perpendicular distance of a point from an axis. As
with `distanceFrom`/`squaredDistanceFrom` this is slightly more efficient than
`radialDistanceFrom` since it avoids a square root.
-}
squaredRadialDistanceFrom : Axis3d -> Point3d -> Float
squaredRadialDistanceFrom axis point =
    vectorFrom (Axis3d.originPoint axis) point
        |> Vector3d.crossProduct (Direction3d.toVector (Axis3d.direction axis))
        |> Vector3d.squaredLength


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane.

    plane =
        Plane3d
            { originPoint = Point3d ( 1, 2, 3 )
            , normalDirection = Direction3d.y
            }

    point =
        Point3d ( 3, 3, 3 )

    Point3d.signedDistanceFrom plane point
    --> 1

    Point3d.signedDistanceFrom plane Point3d.origin
    --> -2

This means that flipping a plane (reversing its normal direction) will also flip
the sign of the result of this function:

    flippedPlane =
        Plane3d.flip plane

    Point3d.signedDistanceFrom flippedPlane point
    --> -1

-}
signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (Plane3d.originPoint plane)

        ( nx, ny, nz ) =
            Direction3d.components (Plane3d.normalDirection plane)
    in
        (x - x0) * nx + (y - y0) * ny + (z - z0) * nz


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point3d ( 1, 1, 1 )

    point =
        Point3d ( 1, 2, 3 )

    Point3d.scaleAbout centerPoint 3 point
    --> Point3d ( 1, 4, 7 )

    Point3d.scaleAbout centerPoint 0.5 point
    --> Point3d ( 1, 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale point =
    vectorFrom centerPoint point |> Vector3d.scaleBy scale |> addTo centerPoint


{-| Rotate a point around an axis by a given angle (in radians).

    axis =
        Axis3d.x

    angle =
        degrees 45

    point =
        Point3d ( 3, 1, 0 )

    Point3d.rotateAround axis angle point
    --> Point3d ( 3, 0.7071, 0.7071 )

Rotation direction is given by the right-hand rule, counterclockwise around the
direction of the axis.

-}
rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround axis angle point =
    let
        originPoint =
            Axis3d.originPoint axis
    in
        vectorFrom originPoint point
            |> Vector3d.rotateAround axis angle
            |> addTo originPoint


{-| Translate a point by a given displacement.

    point =
        Point3d ( 3, 4, 5 )

    displacement =
        Vector3d ( 1, 2, 3 )

    Point3d.translateBy displacement point
    --> Point3d ( 4, 6, 8 )

-}
translateBy : Vector3d -> Point3d -> Point3d
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
        Point3d ( px + vx, py + vy, pz + vz )


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    point =
        Point3d ( 1, 2, 3 )

    -- Plane3d.xy is the plane Z=0
    Point3d.mirrorAcross Plane3d.xy point
    --> Point3d ( 1, 2, -3 )

    -- Plane3d.yz is the plane X=0
    Point3d.mirrorAcross Plane3d.yz point
    --> Point3d ( -1, 2, 3 )

The plane does not have to pass through the origin:

    -- offsetPlane is the plane Z=1
    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin
    --> Point3d ( 0, 0, 2 )

-}
mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane point =
    let
        originPoint =
            Plane3d.originPoint plane
    in
        vectorFrom originPoint point
            |> Vector3d.mirrorAcross plane
            |> addTo originPoint


{-| Project a point perpendicularly onto a plane.

    point =
        Point3d ( 1, 2, 3 )

    Point3d.projectOnto Plane3d.xy point
    --> Point3d ( 1, 2, 0 )

    Point3d.projectOnto Plane3d.yz point
    --> Point3d ( 0, 2, 3 )

The plane does not have to pass through the origin:

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane point
    --> Point3d ( 1, 2, 1 )

-}
projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        signedDistance =
            signedDistanceFrom plane point

        displacement =
            Vector3d.in_ (Plane3d.normalDirection plane) -signedDistance
    in
        translateBy displacement point


{-| Project a point perpendicularly (radially) onto an axis.

    point =
        Point3d ( 1, 2, 3 )

    Point3d.projectRadiallyOnto Axis3d.x
    --> Point3d ( 1, 0, 0 )

    verticalAxis =
        Axis3d
            { originPoint = Point3d ( 0, 1, 2 )
            , direction = Direction3d.z
            }

    Point3d.projectRadiallyOnto verticalAxis
    --> Point3d ( 0, 1, 3 )

-}
projectRadiallyOnto : Axis3d -> Point3d -> Point3d
projectRadiallyOnto axis point =
    along axis (distanceAlong axis point)


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Point3d.relativeTo localFrame (Point3d ( 4, 5, 6 ))
    --> Point3d ( 3, 3, 3 )

    Point3d.relativeTo localFrame (Point3d ( 1, 1, 1 ))
    --> Point3d ( 0, -1, -2 )

-}
relativeTo : Frame3d -> Point3d -> Point3d
relativeTo frame point =
    vectorFrom (Frame3d.originPoint frame) point
        |> Vector3d.relativeTo frame
        |> Vector3d.components
        |> Point3d


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Point3d.placeIn localFrame (Point3d ( 3, 3, 3 ))
    --> Point3d ( 4, 5, 6 )

    Point3d.placeIn localFrame (Point3d ( 0, -1, -2 ))
    --> Point3d ( 1, 1, 1 )

-}
placeIn : Frame3d -> Point3d -> Point3d
placeIn frame point =
    Vector3d (coordinates point)
        |> Vector3d.placeIn frame
        |> addTo (Frame3d.originPoint frame)


{-| Project a point into a given sketch plane. Conceptually, this projects the
point onto the plane and then expresses the projected point in 2D sketch
coordinates.

    point =
        Point3d ( 2, 1, 3 )

    Point3d.projectInto SketchPlane3d.xy point
    --> Point2d ( 2, 1 )

    Point3d.projectInto SketchPlane3d.yz point
    --> Point2d ( 1, 3 )

    Point3d.projectInto SketchPlane3d.zx point
    --> Point2d ( 3, 2 )

-}
projectInto : SketchPlane3d -> Point3d -> Point2d
projectInto sketchPlane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        dx =
            x - x0

        dy =
            y - y0

        dz =
            z - z0
    in
        Point2d
            ( dx * ux + dy * uy + dz * uz
            , dx * vx + dy * vy + dz * vz
            )


{-| Construct a bounding box containing both of the given points.

    point1 =
        Point3d ( 2, 1, 3 )

    point2 =
        Point3d ( -1, 5, -2 )

    Point3d.hull point1 point2
    --> BoundingBox3d
    -->     { minX = -1
    -->     , maxX = 2
    -->     , minY = 1
    -->     , maxY = 5
    -->     , minZ = -2
    -->     , maxZ = 3
    -->     }

-}
hull : Point3d -> Point3d -> BoundingBox3d
hull firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            coordinates firstPoint

        ( x2, y2, z2 ) =
            coordinates secondPoint
    in
        BoundingBox3d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            , minZ = min z1 z2
            , maxZ = max z1 z2
            }
