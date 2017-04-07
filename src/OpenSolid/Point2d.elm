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


module OpenSolid.Point2d
    exposing
        ( origin
        , polar
        , midpoint
        , interpolateFrom
        , interpolate
        , along
        , in_
        , coordinates
        , xCoordinate
        , yCoordinate
        , equalWithin
        , vectorFrom
        , directionFrom
        , distanceFrom
        , squaredDistanceFrom
        , distanceAlong
        , signedDistanceFrom
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , relativeTo
        , placeIn
        , placeOnto
        , hull
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/point2d.svg" alt="Point2d" width="160">

A `Point2d` represents a position in 2D space and is defined by its X and Y
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

The simplest way to construct a `Point2d` value is by passing a tuple of X and Y
coordinates to the `Point2d` constructor, for example

    point =
        Point2d ( 2, 3 )


# Predefined points

@docs origin


# Constructors

@docs polar, midpoint, interpolateFrom, interpolate, along, in_


# Coordinates

@docs coordinates, xCoordinate, yCoordinate


# Comparison

@docs equalWithin


# Displacement and distance

@docs vectorFrom, directionFrom, distanceFrom, squaredDistanceFrom, distanceAlong, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto


# Coordinate frames

Functions for transforming points between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto


# Bounds

@docs hull

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Scalar as Scalar
import OpenSolid.Bootstrap.Axis2d as Axis2d
import OpenSolid.Bootstrap.Frame2d as Frame2d
import OpenSolid.Bootstrap.Direction3d as Direction3d
import OpenSolid.Bootstrap.Point3d as Point3d
import OpenSolid.Bootstrap.SketchPlane3d as SketchPlane3d


addTo : Point2d -> Vector2d -> Point2d
addTo point vector =
    translateBy vector point


{-| The point (0, 0).

    Point2d.origin
    --> Point2d ( 0, 0 )

-}
origin : Point2d
origin =
    Point2d ( 0, 0 )


{-| Construct a point from a radius and angle. Radius is measured from the
origin and angle is measured counterclockwise from the positive X direction.

    Point2d.polar ( 2, degrees 135 )
    --> Point2d ( -1.4142, 1.4142 )

This is shorthand for using Elm's built-in [`fromPolar`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#fromPolar)
function and passing the result to the `Point2d` constructor:

    Point2d.polar ( r, theta )

is equivalent to

    Point2d (fromPolar ( r, theta ))

-}
polar : ( Float, Float ) -> Point2d
polar coordinates =
    Point2d (fromPolar coordinates)


{-| Construct a point halfway between two other points.

    p1 =
        Point2d ( 1, 1 )

    p2 =
        Point2d ( 3, 7 )

    Point2d.midpoint p1 p2
    --> Point2d ( 2, 4 )

-}
midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point2d.origin

    endPoint =
        Point2d ( 8, 12 )

    Point2d.interpolateFrom startPoint endPoint 0.25
    --> Point2d ( 2, 3 )

Partial application may be useful:

    interpolatedPoint : Float -> Point2d
    interpolatedPoint =
        Point2d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point2d ( 0, 0 )
    --> , Point2d ( 4, 6 )
    --> , Point2d ( 8, 12 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point2d ( -4, -6 )

    interpolatedPoint 1.25
    --> Point2d ( 10, 15 )

-}
interpolateFrom : Point2d -> Point2d -> Float -> Point2d
interpolateFrom p1 p2 t =
    let
        ( x1, y1 ) =
            coordinates p1

        ( x2, y2 ) =
            coordinates p2
    in
        Point2d
            ( Scalar.interpolateFrom x1 x2 t
            , Scalar.interpolateFrom y1 y2 t
            )


{-| DEPRECATED: Alias for `interpolateFrom`, kept for compatibility. Use
`interpolateFrom` instead.
-}
interpolate : Point2d -> Point2d -> Float -> Point2d
interpolate =
    interpolateFrom


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point2d.along Axis2d.y 3
    --> Point2d ( 0, 3 )

Positive and negative distances will be interpreted relative to the direction of
the axis:

    horizontalAxis =
        Axis2d
            { originPoint = Point2d ( 1, 1 )
            , direction = Direction2d ( -1, 0 )
            }

    Point2d.along horizontalAxis 3
    --> Point2d ( -2, 1 )

    Point2d.along horizontalAxis -3
    --> Point2d ( 4, 1 )

-}
along : Axis2d -> Float -> Point2d
along axis distance =
    Axis2d.originPoint axis
        |> translateBy (Vector2d.in_ (Axis2d.direction axis) distance)


{-| Construct a point given its local coordinates within a particular frame.

    rotatedFrame =
        Frame2d.xy |> Frame2d.rotateBy (degrees 45)

    Point2d.in_ rotatedFrame ( 2, 0 )
    --> Point2d ( 1.4142, 1.4142 )

This is shorthand for using `Point2d.placeIn`;

    Point2d.in_ frame coordinates

is equivalent to

    Point2d coordinates |> Point2d.placeIn frame

-}
in_ : Frame2d -> ( Float, Float ) -> Point2d
in_ frame coordinates =
    placeIn frame (Point2d coordinates)


{-| Get the coordinates of a point as a tuple.

    ( x, y ) =
        Point2d.coordinates point

To get the polar coordinates of a point, you can use Elm's built-in [`toPolar`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toPolar)
function:

    ( radius, angle ) =
        toPolar (Point2d.coordinates point)

-}
coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates_) =
    coordinates_


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d ( 2, 3 ))
    --> 2

-}
xCoordinate : Point2d -> Float
xCoordinate (Point2d ( x, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d ( 2, 3 ))
    --> 3

-}
yCoordinate : Point2d -> Float
yCoordinate (Point2d ( _, y )) =
    y


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point2d ( 1, 2 )

    secondPoint =
        Point2d ( 0.9999, 2.0002 )

    Point2d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point2d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Float -> Point2d -> Point2d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| Find the vector from one point to another.

    startPoint =
        Point2d ( 1, 1 )

    endPoint =
        Point2d ( 4, 5 )

    Point2d.vectorFrom startPoint endPoint
    --> Vector2d ( 3, 4 )

-}
vectorFrom : Point2d -> Point2d -> Vector2d
vectorFrom firstPoint secondPoint =
    let
        ( x1, y1 ) =
            coordinates firstPoint

        ( x2, y2 ) =
            coordinates secondPoint
    in
        Vector2d ( x2 - x1, y2 - y1 )


{-| Attempt to find the direction from the first point to the second. If the two
points are coincident, returns `Nothing`.

    point =
        Point2d ( 1, 1 )

    Point2d.directionFrom Point2d.origin point
    --> Just (Direction2d ( 0.7071, 0.7071 ))

    Point2d.directionFrom point Point2d.origin
    --> Just (Direction2d ( -0.7071, -0.7071 ))

    Point2d.directionFrom point point
    --> Nothing

-}
directionFrom : Point2d -> Point2d -> Maybe Direction2d
directionFrom firstPoint secondPoint =
    Vector2d.direction (vectorFrom firstPoint secondPoint)


{-| Find the distance between two points.

    p1 =
        Point2d ( 2, 3 )

    p2 =
        Point2d ( 5, 7 )

    Point2d.distanceFrom p1 p2
    --> 5

Partial application can be useful:

    points =
        [ Point2d ( 3, 4 )
        , Point2d ( 10, 0 )
        , Point2d ( -1, 2 )
        ]

    distanceFromOrigin : Point2d -> Float
    distanceFromOrigin =
        Point2d.distanceFrom Point2d.origin

    List.sortBy distanceFromOrigin points
    --> [ Point2d ( -1, 2 )
    --> , Point2d ( 3, 4 )
    --> , Point2d ( 10, 0 )
    --> ]

-}
distanceFrom : Point2d -> Point2d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point2d.squaredDistanceFrom p1 p2 > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point2d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (vectorFrom firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis2d
            { originPoint = Point2d ( 1, 2 )
            , direction = Direction2d.x
            }

    point =
        Point2d ( 3, 3 )

    Point2d.distanceAlong axis point
    --> 2

    Point2d.distanceAlong axis Point2d.origin
    --> -1

-}
distanceAlong : Axis2d -> Point2d -> Float
distanceAlong axis point =
    vectorFrom (Axis2d.originPoint axis) point
        |> Vector2d.componentIn (Axis2d.direction axis)


{-| Find the perpendicular distance of a point from an axis. The result
will be positive if the point is to the left of the axis and negative if it is
to the right, with the forwards direction defined by the direction of the axis.

    -- A horizontal axis through a point with a Y
    -- coordinate of 2 is effectively the line Y=2
    axis =
        Axis2d
            { originPoint = Point2d ( 1, 2 )
            , direction = Direction2d.x
            }

    point =
        Point2d ( 3, 3 )

    -- Since the axis is in the positive X direction,
    -- points above the axis are to the left (positive)
    Point2d.signedDistanceFrom axis point
    -->  1

    -- and points below are to the right (negative)
    Point2d.signedDistanceFrom axis Point2d.origin
    --> -2

This means that flipping an axis will also flip the sign of the result of this
function:

    -- Flipping an axis reverses its direction
    flippedAxis =
        Axis2d.flip axis

    Point2d.signedDistanceFrom flippedAxis point
    --> -1

    Point2d.signedDistanceFrom flippedAxis Point2d.origin
    --> 2

-}
signedDistanceFrom : Axis2d -> Point2d -> Float
signedDistanceFrom axis point =
    let
        directionVector =
            Direction2d.toVector (Axis2d.direction axis)

        displacementVector =
            vectorFrom (Axis2d.originPoint axis) point
    in
        Vector2d.crossProduct directionVector displacementVector


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point2d ( 1, 1 )

    point =
        Point2d ( 2, 3 )

    Point2d.scaleAbout centerPoint 3 point
    --> Point2d ( 4, 7 )

    Point2d.scaleAbout centerPoint 0.5 point
    --> Point2d ( 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale point =
    vectorFrom centerPoint point |> Vector2d.scaleBy scale |> addTo centerPoint


{-| Rotate around a given center point counterclockwise by a given angle (in
radians). The point to rotate around is given first and the point to rotate is
given last.

    centerPoint =
        Point2d ( 2, 0 )

    angle =
        degrees 45

    point =
        Point2d ( 3, 0 )

    Point2d.rotateAround centerPoint angle point
    --> Point2d ( 2.7071, 0.7071 )

-}
rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    vectorFrom centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


{-| Translate a point by a given displacement.

    point =
        Point2d ( 3, 4 )

    displacement =
        Vector2d ( 1, 2 )

    Point2d.translateBy displacement point
    --> Point2d ( 4, 6 )

-}
translateBy : Vector2d -> Point2d -> Point2d
translateBy vector point =
    let
        ( vx, vy ) =
            Vector2d.components vector

        ( px, py ) =
            coordinates point
    in
        Point2d ( px + vx, py + vy )


{-| Mirror a point across an axis. The result will be the same distance from the
axis but on the opposite side.

    point =
        Point2d ( 2, 3 )

    Point2d.mirrorAcross Axis2d.x point
    --> Point2d ( 2, -3 )

    Point2d.mirrorAcross Axis2d.y point
    --> Point2d ( -2, 3 )

-}
mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    vectorFrom (Axis2d.originPoint axis)
        >> Vector2d.mirrorAcross axis
        >> addTo (Axis2d.originPoint axis)


{-| Project a point perpendicularly onto an axis.

    point =
        Point2d ( 2, 3 )

    Point2d.projectOnto Axis2d.x point
    --> Point2d ( 2, 0 )

    Point2d.projectOnto Axis2d.y point
    --> Point2d ( 0, 3 )

The axis does not have to pass through the origin:

    offsetYAxis =
        Axis2d
            { originPoint = Point2d ( 1, 0 )
            , direction = Direction2d.y
            }

    Point2d.projectOnto offsetYAxis point
    --> Point2d ( 1, 3 )

-}
projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    vectorFrom (Axis2d.originPoint axis)
        >> Vector2d.projectOnto axis
        >> addTo (Axis2d.originPoint axis)


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Point2d.relativeTo localFrame (Point2d ( 4, 5 ))
    --> Point2d ( 3, 3 )

    Point2d.relativeTo localFrame (Point2d ( 1, 1 ))
    --> Point2d ( 0, -1 )

-}
relativeTo : Frame2d -> Point2d -> Point2d
relativeTo frame point =
    vectorFrom (Frame2d.originPoint frame) point
        |> Vector2d.relativeTo frame
        |> Vector2d.components
        |> Point2d


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Point2d.placeIn localFrame (Point2d ( 3, 3 ))
    --> Point2d ( 4, 5 )

    Point2d.placeIn localFrame (Point2d ( 0, 1 ))
    --> Point2d ( 1, 1 )

-}
placeIn : Frame2d -> Point2d -> Point2d
placeIn frame point =
    Vector2d (coordinates point)
        |> Vector2d.placeIn frame
        |> addTo (Frame2d.originPoint frame)


{-| Take a point defined in 2D coordinates within a particular sketch plane and
return the corresponding point in 3D.

    point =
        Point2d ( 2, 1 )

    Point2d.placeOnto SketchPlane3d.xy point
    --> Point3d ( 2, 1, 0 )

    Point2d.placeOnto SketchPlane3d.xz point
    --> Point3d ( 2, 0, 1 )

The sketch plane can have any position and orientation:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x (degrees 45)

    Point2d.placeOnto tiltedSketchPlane point
    --> Point3d ( 2, 0.7071, 0.7071 )

-}
placeOnto : SketchPlane3d -> Point2d -> Point3d
placeOnto sketchPlane =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)
    in
        \(Point2d ( x, y )) ->
            Point3d
                ( x0 + x * ux + y * vx
                , y0 + x * uy + y * vy
                , z0 + x * uz + y * vz
                )


{-| Construct a bounding box containing both of the given points.

    point1 =
        Point2d ( 2, 3 )

    point2 =
        Point2d ( -1, 5 )

    Point2d.hull point1 point2
    --> BoundingBox2d
    -->     { minX = -1
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 5
    -->     }

-}
hull : Point2d -> Point2d -> BoundingBox2d
hull firstPoint secondPoint =
    let
        ( x1, y1 ) =
            coordinates firstPoint

        ( x2, y2 ) =
            coordinates secondPoint
    in
        BoundingBox2d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            }
