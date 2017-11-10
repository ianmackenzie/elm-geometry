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
        ( Point2d
        , along
        , circumcenter
        , coordinates
        , distanceAlong
        , distanceFrom
        , equalWithin
        , fromCoordinates
        , fromPolarCoordinates
        , hull
        , hullOf
        , in_
        , interpolateFrom
        , midpoint
        , mirrorAcross
        , origin
        , placeIn
        , polarCoordinates
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , signedDistanceAlong
        , signedDistanceFrom
        , squaredDistanceFrom
        , translateBy
        , xCoordinate
        , yCoordinate
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/point2d.svg" alt="Point2d" width="160">

A `Point2d` represents a position in 2D space and is defined by its X and Y
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point2d


# Constants

@docs origin


# Constructors

@docs fromCoordinates, fromPolarCoordinates, midpoint, interpolateFrom, along, in_, circumcenter


# Properties

@docs coordinates, xCoordinate, yCoordinate, polarCoordinates


# Comparison

@docs equalWithin


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, distanceAlong, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto


# Coordinate conversions

Functions for transforming points between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn


# Bounding box construction

@docs hull, hullOf

-}

import OpenSolid.Bootstrap.Axis2d as Axis2d
import OpenSolid.Bootstrap.Frame2d as Frame2d
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Geometry.Internal as Internal exposing (Axis2d, Frame2d)
import OpenSolid.Scalar as Scalar
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


addTo : Point2d -> Vector2d -> Point2d
addTo point vector =
    translateBy vector point


{-| -}
type alias Point2d =
    Internal.Point2d


{-| The point (0, 0).

    Point2d.origin
    --> Point2d.fromCoordinates ( 0, 0 )

-}
origin : Point2d
origin =
    fromCoordinates ( 0, 0 )


{-| Construct a point from its X and Y coordinates.

    point =
        Point2d.fromCoordinates ( 2, 3 )

-}
fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates =
    Internal.Point2d


{-| Construct a point from a radius and angle. Radius is measured from the
origin and angle is measured counterclockwise from the positive X direction.

    Point2d.fromPolarCoordinates ( 2, degrees 135 )
    --> Point2d.fromCoordinates ( -1.4142, 1.4142 )

-}
fromPolarCoordinates : ( Float, Float ) -> Point2d
fromPolarCoordinates coordinates =
    fromCoordinates (fromPolar coordinates)


{-| Construct a point halfway between two other points.

    p1 =
        Point2d.fromCoordinates ( 1, 1 )

    p2 =
        Point2d.fromCoordinates ( 3, 7 )

    Point2d.midpoint p1 p2
    --> Point2d.fromCoordinates ( 2, 4 )

-}
midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point2d.origin

    endPoint =
        Point2d.fromCoordinates ( 8, 12 )

    Point2d.interpolateFrom startPoint endPoint 0.25
    --> Point2d.fromCoordinates ( 2, 3 )

Partial application may be useful:

    interpolatedPoint : Float -> Point2d
    interpolatedPoint =
        Point2d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 4, 6 )
    --> , Point2d.fromCoordinates ( 8, 12 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point2d.fromCoordinates ( -4, -6 )

    interpolatedPoint 1.25
    --> Point2d.fromCoordinates ( 10, 15 )

-}
interpolateFrom : Point2d -> Point2d -> Float -> Point2d
interpolateFrom p1 p2 t =
    let
        ( x1, y1 ) =
            coordinates p1

        ( x2, y2 ) =
            coordinates p2
    in
    fromCoordinates
        ( Scalar.interpolateFrom x1 x2 t
        , Scalar.interpolateFrom y1 y2 t
        )


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point2d.along Axis2d.y 3
    --> Point2d.fromCoordinates ( 0, 3 )

Positive and negative distances will be interpreted relative to the direction of
the axis:

    horizontalAxis =
        Axis2d.with
            { originPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , direction = Direction2d.negativeX
            }

    Point2d.along horizontalAxis 3
    --> Point2d.fromCoordinates ( -2, 1 )

    Point2d.along horizontalAxis -3
    --> Point2d.fromCoordinates ( 4, 1 )

-}
along : Axis2d -> Float -> Point2d
along axis distance =
    Axis2d.originPoint axis
        |> translateBy
            (Vector2d.with
                { length = distance
                , direction = Axis2d.direction axis
                }
            )


{-| Construct a point given its local coordinates within a particular frame.

    rotatedFrame =
        Frame2d.xy |> Frame2d.rotateBy (degrees 45)

    Point2d.in_ rotatedFrame ( 2, 0 )
    --> Point2d.fromCoordinates ( 1.4142, 1.4142 )

This is shorthand for using `Point2d.placeIn`;

    Point2d.in_ frame coordinates

is equivalent to

    Point2d.placeIn frame
        (Point2d.fromCoordinates coordinates)

-}
in_ : Frame2d -> ( Float, Float ) -> Point2d
in_ frame coordinates =
    placeIn frame (fromCoordinates coordinates)


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point2d.circumcenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        , Point2d.fromCoordinates ( 0, 1 )
        )
    --> Just (Point2d.fromCoordinates ( 0.5, 0.5 ))

    Point2d.circumcenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 1 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Just (Point2d.fromCoordinates ( 2, -1.5 ))

    Point2d.circumCenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 0 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Nothing

    Point2d.circumCenter
        ( Point2d.origin
        , Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        )
    --> Nothing

-}
circumcenter : ( Point2d, Point2d, Point2d ) -> Maybe Point2d
circumcenter ( p1, p2, p3 ) =
    let
        a2 =
            squaredDistanceFrom p1 p2

        b2 =
            squaredDistanceFrom p2 p3

        c2 =
            squaredDistanceFrom p3 p1

        t1 =
            a2 * (b2 + c2 - a2)

        t2 =
            b2 * (c2 + a2 - b2)

        t3 =
            c2 * (a2 + b2 - c2)

        sum =
            t1 + t2 + t3
    in
    if sum == 0 then
        Nothing
    else
        let
            w1 =
                t1 / sum

            w2 =
                t2 / sum

            w3 =
                t3 / sum

            ( x1, y1 ) =
                coordinates p1

            ( x2, y2 ) =
                coordinates p2

            ( x3, y3 ) =
                coordinates p3
        in
        Just <|
            fromCoordinates
                ( w1 * x3 + w2 * x1 + w3 * x2
                , w1 * y3 + w2 * y1 + w3 * y2
                )


{-| Get the coordinates of a point as a tuple.

    ( x, y ) =
        Point2d.coordinates point

-}
coordinates : Point2d -> ( Float, Float )
coordinates (Internal.Point2d coordinates_) =
    coordinates_


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d.fromCoordinates ( 2, 3 ))
    --> 2

-}
xCoordinate : Point2d -> Float
xCoordinate (Internal.Point2d ( x, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d.fromCoordinates ( 2, 3 ))
    --> 3

-}
yCoordinate : Point2d -> Float
yCoordinate (Internal.Point2d ( _, y )) =
    y


{-| Get the polar coordinates (radius and polar angle) of a point.

    Point2d.polarCoordinates
        (Point2d.fromCoordinates ( 1, 1 ))
    --> ( 1.4142, degrees 45 )

-}
polarCoordinates : Point2d -> ( Float, Float )
polarCoordinates point =
    toPolar (coordinates point)


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point2d.fromCoordinates ( 1, 2 )

    secondPoint =
        Point2d.fromCoordinates ( 0.9999, 2.0002 )

    Point2d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point2d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Float -> Point2d -> Point2d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| Find the distance from the first point to the second.

    p1 =
        Point2d.fromCoordinates ( 2, 3 )

    p2 =
        Point2d.fromCoordinates ( 5, 7 )

    Point2d.distanceFrom p1 p2
    --> 5

Partial application can be useful:

    points =
        [ Point2d.fromCoordinates ( 3, 4 )
        , Point2d.fromCoordinates ( 10, 0 )
        , Point2d.fromCoordinates ( -1, 2 )
        ]

    points
        |> List.sortBy
            (Point2d.distanceFrom Point2d.origin)
    --> [ Point2d.fromCoordinates ( -1, 2 )
    --> , Point2d.fromCoordinates ( 3, 4 )
    --> , Point2d.fromCoordinates ( 10, 0 )
    --> ]

-}
distanceFrom : Point2d -> Point2d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point2d.squaredDistanceFrom p1 p2
        > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point2d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (Vector2d.from firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis2d.with
            { originPoint =
                Point2d.fromCoordinates ( 1, 2 )
            , direction = Direction2d.x
            }

    point =
        Point2d.fromCoordinates ( 3, 3 )

    Point2d.signedDistanceAlong axis point
    --> 2

    Point2d.signedDistanceAlong axis Point2d.origin
    --> -1

-}
signedDistanceAlong : Axis2d -> Point2d -> Float
signedDistanceAlong axis point =
    Vector2d.from (Axis2d.originPoint axis) point
        |> Vector2d.componentIn (Axis2d.direction axis)


{-| DEPRECATED: Alias for `signedDistanceAlong`, kept for compatibility. Use
`signedDistanceAlong` instead.
-}
distanceAlong : Axis2d -> Point2d -> Float
distanceAlong =
    signedDistanceAlong


{-| Find the perpendicular distance of a point from an axis. The result
will be positive if the point is to the left of the axis and negative if it is
to the right, with the forwards direction defined by the direction of the axis.

    -- A horizontal axis through a point with a Y
    -- coordinate of 2 is effectively the line Y=2
    axis =
        Axis2d.with
            { originPoint =
                Point2d.fromCoordinates ( 1, 2 )
            , direction = Direction2d.x
            }

    point =
        Point2d.fromCoordinates ( 3, 3 )

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
            Vector2d.from (Axis2d.originPoint axis) point
    in
    Vector2d.crossProduct directionVector displacementVector


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point2d.fromCoordinates ( 1, 1 )

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.scaleAbout centerPoint 3 point
    --> Point2d.fromCoordinates ( 4, 7 )

    Point2d.scaleAbout centerPoint 0.5 point
    --> Point2d.fromCoordinates ( 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale point =
    Vector2d.from centerPoint point
        |> Vector2d.scaleBy scale
        |> addTo centerPoint


{-| Rotate around a given center point counterclockwise by a given angle (in
radians). The point to rotate around is given first and the point to rotate is
given last.

    centerPoint =
        Point2d.fromCoordinates ( 2, 0 )

    angle =
        degrees 45

    point =
        Point2d.fromCoordinates ( 3, 0 )

    Point2d.rotateAround centerPoint angle point
    --> Point2d.fromCoordinates ( 2.7071, 0.7071 )

-}
rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    Vector2d.from centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


{-| Translate a point by a given displacement.

    point =
        Point2d.fromCoordinates ( 3, 4 )

    displacement =
        Vector2d.fromComponents ( 1, 2 )

    Point2d.translateBy displacement point
    --> Point2d.fromCoordinates ( 4, 6 )

-}
translateBy : Vector2d -> Point2d -> Point2d
translateBy vector point =
    let
        ( vx, vy ) =
            Vector2d.components vector

        ( px, py ) =
            coordinates point
    in
    fromCoordinates ( px + vx, py + vy )


{-| Mirror a point across an axis. The result will be the same distance from the
axis but on the opposite side.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.mirrorAcross Axis2d.x point
    --> Point2d.fromCoordinates ( 2, -3 )

    Point2d.mirrorAcross Axis2d.y point
    --> Point2d.fromCoordinates ( -2, 3 )

-}
mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    Vector2d.from (Axis2d.originPoint axis)
        >> Vector2d.mirrorAcross axis
        >> addTo (Axis2d.originPoint axis)


{-| Project a point perpendicularly onto an axis.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.projectOnto Axis2d.x point
    --> Point2d.fromCoordinates ( 2, 0 )

    Point2d.projectOnto Axis2d.y point
    --> Point2d.fromCoordinates ( 0, 3 )

The axis does not have to pass through the origin:

    offsetYAxis =
        Axis2d.with
            { originPoint =
                Point2d.fromCoordinates ( 1, 0 )
            , direction = Direction2d.y
            }

    Point2d.projectOnto offsetYAxis point
    --> Point2d.fromCoordinates ( 1, 3 )

-}
projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    Vector2d.from (Axis2d.originPoint axis)
        >> Vector2d.projectOnto axis
        >> addTo (Axis2d.originPoint axis)


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Point2d.relativeTo localFrame
        (Point2d.fromCoordinates ( 4, 5 ))
    --> Point2d.fromCoordinates ( 3, 3 )

    Point2d.relativeTo localFrame
        (Point2d.fromCoordinates ( 1, 1 ))
    --> Point2d.fromCoordinates ( 0, -1 )

-}
relativeTo : Frame2d -> Point2d -> Point2d
relativeTo frame point =
    Vector2d.from (Frame2d.originPoint frame) point
        |> Vector2d.relativeTo frame
        |> Vector2d.components
        |> fromCoordinates


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Point2d.placeIn localFrame
        (Point2d.fromCoordinates ( 3, 3 ))
    --> Point2d.fromCoordinates ( 4, 5 )

    Point2d.placeIn localFrame
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Point2d.fromCoordinates ( 1, 1 )

-}
placeIn : Frame2d -> Point2d -> Point2d
placeIn frame point =
    Vector2d.fromComponents (coordinates point)
        |> Vector2d.placeIn frame
        |> addTo (Frame2d.originPoint frame)


{-| Construct a bounding box containing both of the given points.

    point1 =
        Point2d.fromCoordinates ( 2, 3 )

    point2 =
        Point2d.fromCoordinates ( -1, 5 )

    Point2d.hull point1 point2
    --> BoundingBox2d.with
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
    BoundingBox2d.with
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        }


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    points =
        [ Point2d.fromCoordinates ( 2, 3 )
        , Point2d.fromCoordinates ( -1, 5 )
        , Point2d.fromCoordinates ( 6, 4 )
        ]

    Point2d.hullOf points
    --> Just
    -->     (BoundingBox2d.with
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 3
    -->         , maxY = 5
    -->         }
    -->     )

    Point2d.hullOf []
    --> Nothing

-}
hullOf : List Point2d -> Maybe BoundingBox2d
hullOf points =
    BoundingBox2d.hullOf (List.map BoundingBox2d.singleton points)
