--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point3d exposing
    ( Point3d
    , origin
    , meters, pixels
    , xyz, xyzIn, midpoint, interpolateFrom, along, on, xyOn, rThetaOn, circumcenter
    , fromTuple, toTuple, fromRecord, toRecord
    , xCoordinate, yCoordinate, zCoordinate, xCoordinateIn, yCoordinateIn, zCoordinateIn
    , equalWithin, lexicographicComparison
    , distanceFrom, signedDistanceAlong, distanceFromAxis, signedDistanceFrom
    , centroid, centroidOf, centroid3, centroid4, centroidN
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis
    , relativeTo, placeIn, projectInto
    )

{-| A `Point3d` represents a position in 3D space and is defined by its X, Y and
Z coordinates. This module contains a variety of point-related functionality,
such as

  - Measuring distance between points, or the distance of a point from an axis
    or a plane
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point3d


# Constants

@docs origin


# Literals

@docs meters, pixels


# Constructors

@docs xyz, xyzIn, midpoint, interpolateFrom, along, on, xyOn, rThetaOn, circumcenter


# Interop

These functions are useful for interoperability with other Elm code that uses
plain `Float` tuples or records to represent points. The resulting `Point3d`
values will have [unitless](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#unitless-quantities)
coordinates.

@docs fromTuple, toTuple, fromRecord, toRecord


# Properties

@docs coordinates, coordinatesIn, xCoordinate, yCoordinate, zCoordinate, xCoordinateIn, yCoordinateIn, zCoordinateIn


# Comparison

@docs equalWithin, lexicographicComparison


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, distanceFromAxis, squaredDistanceFromAxis, signedDistanceFrom


# Centroid calculation

@docs centroid, centroidOf, centroid3, centroid4, centroidN


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, SketchPlane3d)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Squared, Unitless)
import Quantity.Extra as Quantity
import Vector3d exposing (Vector3d)


{-| -}
type alias Point3d units coordinates =
    Types.Point3d units coordinates


{-| The point (0, 0, 0).

    Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 0 )

-}
origin : Point3d units coordinates
origin =
    Types.Point3d
        { x = 0
        , y = 0
        , z = 0
        }


{-| Construct a point from its X, Y and Z coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

-}
xyz : Quantity Float units -> Quantity Float units -> Quantity Float units -> Point3d units coordinates
xyz (Quantity x) (Quantity y) (Quantity z) =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| TODO
-}
meters : Float -> Float -> Float -> Point3d Meters coordinates
meters x y z =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| TODO
-}
pixels : Float -> Float -> Float -> Point3d Pixels coordinates
pixels x y z =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| Construct a point halfway between two other points.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 1 )

    p2 =
        Point3d.fromCoordinates ( 3, 7, 9 )

    Point3d.midpoint p1 p2
    --> Point3d.fromCoordinates ( 2, 4, 5 )

-}
midpoint : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
midpoint (Types.Point3d p1) (Types.Point3d p2) =
    Types.Point3d
        { x = p1.x + 0.5 * (p2.x - p1.x)
        , y = p1.y + 0.5 * (p2.y - p1.y)
        , z = p1.z + 0.5 * (p2.z - p1.z)
        }


{-| TODO
-}
centroid : Point3d units coordinates -> List (Point3d units coordinates) -> Point3d units coordinates
centroid (Types.Point3d p0) rest =
    centroidHelp p0.x p0.y p0.z 1 0 0 0 rest


centroidHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> List (Point3d units coordinates) -> Point3d units coordinates
centroidHelp x0 y0 z0 count dx dy dz points =
    case points of
        (Types.Point3d p) :: remaining ->
            centroidHelp
                x0
                y0
                z0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                (dz + (p.z - z0))
                remaining

        [] ->
            Types.Point3d
                { x = x0 + dx / count
                , y = y0 + dy / count
                , z = z0 + dz / count
                }


{-| TODO
-}
centroidOf : (a -> Point3d units coordinates) -> a -> List a -> Point3d units coordinates
centroidOf toPoint first rest =
    let
        (Types.Point3d p0) =
            toPoint first
    in
    centroidOfHelp toPoint p0.x p0.y p0.z 1 0 0 0 rest


centroidOfHelp : (a -> Point3d units coordinates) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List a -> Point3d units coordinates
centroidOfHelp toPoint x0 y0 z0 count dx dy dz values =
    case values of
        next :: remaining ->
            let
                (Types.Point3d p) =
                    toPoint next
            in
            centroidOfHelp
                toPoint
                x0
                y0
                z0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                (dz + (p.z - z0))
                remaining

        [] ->
            Types.Point3d
                { x = x0 + dx / count
                , y = y0 + dy / count
                , z = z0 + dz / count
                }


{-| TODO
-}
centroid3 : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
centroid3 (Types.Point3d p1) (Types.Point3d p2) (Types.Point3d p3) =
    Types.Point3d
        { x = p1.x + (p2.x - p1.x) / 3 + (p3.x - p1.x) / 3
        , y = p1.y + (p2.y - p1.y) / 3 + (p3.y - p1.y) / 3
        , z = p1.z + (p2.z - p1.z) / 3 + (p3.z - p1.z) / 3
        }


{-| TODO
-}
centroid4 : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
centroid4 (Types.Point3d p1) (Types.Point3d p2) (Types.Point3d p3) (Types.Point3d p4) =
    Types.Point3d
        { x = p1.x + (p2.x - p1.x) / 4 + (p3.x - p1.x) / 4 + (p4.x - p1.x) / 4
        , y = p1.y + (p2.y - p1.y) / 4 + (p3.y - p1.y) / 4 + (p4.y - p1.y) / 4
        , z = p1.z + (p2.z - p1.z) / 4 + (p3.z - p1.z) / 4 + (p4.z - p1.z) / 4
        }


{-| TODO
-}
centroidN : List (Point3d units coordinates) -> Maybe (Point3d units coordinates)
centroidN points =
    case points of
        first :: rest ->
            Just (centroid first rest)

        [] ->
            Nothing


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point3d.fromCoordinates ( 1, 2, 4 )

    endPoint =
        Point3d.fromCoordinates ( 1, 2, 8 )

    Point3d.interpolateFrom startPoint endPoint 0.25
    --> Point3d.fromCoordinates ( 1, 2, 5 )

Partial application may be useful:

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point3d.fromCoordinates ( 1, 2, 4 )
    --> , Point3d.fromCoordinates ( 1, 2, 6 )
    --> , Point3d.fromCoordinates ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point3d.fromCoordinates ( 1, 2, 2 )

    interpolatedPoint 1.25
    --> Point3d.fromCoordinates ( 1, 2, 9 )

-}
interpolateFrom : Point3d units coordinates -> Point3d units coordinates -> Float -> Point3d units coordinates
interpolateFrom (Types.Point3d p1) (Types.Point3d p2) t =
    if t <= 0.5 then
        Types.Point3d
            { x = p1.x + t * (p2.x - p1.x)
            , y = p1.y + t * (p2.y - p1.y)
            , z = p1.z + t * (p2.z - p1.z)
            }

    else
        Types.Point3d
            { x = p2.x + (1 - t) * (p1.x - p2.x)
            , y = p2.y + (1 - t) * (p1.y - p2.y)
            , z = p2.z + (1 - t) * (p1.z - p2.z)
            }


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point3d.along Axis3d.z 2
    --> Point3d.fromCoordinates ( 0, 0, 2 )

Positive and negative distances are interpreted relative to the direction of the
axis:

    horizontalAxis =
        Axis3d.withDirection Direction3d.negativeX
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.along horizontalAxis 3
    --> Point3d.fromCoordinates ( -2, 1, 1 )

    Point3d.along horizontalAxis -3
    --> Point3d.fromCoordinates ( 4, 1, 1 )

-}
along : Axis3d units coordinates -> Quantity Float units -> Point3d units coordinates
along (Types.Axis3d axis) (Quantity distance) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction
    in
    Types.Point3d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        , z = p0.z + distance * d.z
        }


{-| Construct a 3D point lying _on_ a sketch plane by providing a 2D point
specified in XY coordinates _within_ the sketch plane.

    Point3d.on SketchPlane3d.xy <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 1, 0 )

    Point3d.on SketchPlane3d.xz <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 0, 1 )

The sketch plane can have any position and orientation:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (degrees 45)
            |> SketchPlane3d.moveTo
                (Point3d.fromCoordinates ( 10, 10, 10 ))

    Point3d.on tiltedSketchPlane <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 12, 10.7071, 10.7071 )

-}
on : SketchPlane3d units coordinates3d coordinates2d -> Point2d units coordinates2d -> Point3d units coordinates3d
on (Types.SketchPlane3d sketchPlane) (Types.Point2d p) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Point3d
        { x = p0.x + p.x * i.x + p.y * j.x
        , y = p0.y + p.x * i.y + p.y * j.y
        , z = p0.z + p.x * i.z + p.y * j.z
        }


{-| Construct a 3D point lying on a sketch plane by providing its 2D coordinates within that sketch
plane:

    Point3d.fromCoordinatesOn SketchPlane3d.xy
        (meters 2)
        (meters 1)
    --> Point3d.fromCoordinates
    -->     (meters 2)
    -->     (meters 1)
    -->     (meters 0)

    Point3d.fromCoordinatesOn SketchPlane3d.xz
        (meters 2)
        (meters 1)
    --> Point3d.fromCoordinates
    -->     (meters 2)
    -->     (meters 0)
    -->     (meters 1)

-}
xyOn : SketchPlane3d units coordinates3d coordinates2d -> Quantity Float units -> Quantity Float units -> Point3d units coordinates
xyOn (Types.SketchPlane3d sketchPlane) (Quantity x) (Quantity y) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        , z = p0.z + x * i.z + y * j.z
        }


{-| TODO
-}
rThetaOn : SketchPlane3d units coordinates3d coordinates2d -> Quantity Float units -> Angle -> Point3d units coordinates
rThetaOn (Types.SketchPlane3d sketchPlane) (Quantity r) (Quantity theta) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        x =
            r * cos theta

        y =
            r * sin theta
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        , z = p0.z + x * i.z + y * j.z
        }


{-| Construct a point given its local coordinates within a particular frame:

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.fromCoordinatesIn frame ( 1, 2, 3 )
    --> Point3d.fromCoordinates ( 2, 3, 4 )

-}
xyzIn : Frame3d units globalCoordinates localCoordinates -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Point3d units globalCoordinates
xyzIn (Types.Frame3d frame) (Quantity x) (Quantity y) (Quantity z) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x + z * k.x
        , y = p0.y + x * i.y + y * j.y + z * k.y
        , z = p0.z + x * i.z + y * j.z + z * k.z
        }


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point3d.circumcenter
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 0, 1, 0 ))
        (Point3d.fromCoordinates ( 0, 0, 1 ))
    --> Just (Point3d.fromCoordinates (0.33, 0.33, 0.33))

    Point3d.circumcenter
        Point3d.origin
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 2, 0, 0 ))
    --> Nothing

-}
circumcenter : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Point3d units coordinates)
circumcenter (Types.Point3d p1) (Types.Point3d p2) (Types.Point3d p3) =
    let
        ax =
            p2.x - p1.x

        ay =
            p2.y - p1.y

        az =
            p2.z - p1.z

        bx =
            p3.x - p2.x

        by =
            p3.y - p2.y

        bz =
            p3.z - p2.z

        cx =
            p1.x - p3.x

        cy =
            p1.y - p3.y

        cz =
            p1.z - p3.z

        a2 =
            ax * ax + ay * ay + az * az

        b2 =
            bx * bx + by * by + bz * bz

        c2 =
            cx * cx + cy * cy + cz * cz

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
        in
        Just <|
            Types.Point3d
                { x = w1 * p3.x + w2 * p1.x + w3 * p2.x
                , y = w1 * p3.y + w2 * p1.y + w3 * p2.y
                , z = w1 * p3.z + w2 * p1.z + w3 * p2.z
                }


{-| Construct a `Point3d` from a tuple of `Float` values, by specifying what units those values are
in.

    Point3d.fromTuple Length.meters ( 2, 3, 1 )
    --> Point3d.fromCoordinates
    -->     (Length.meters 2)
    -->     (Length.meters 3)
    -->     (Length.meters 1)

-}
fromTuple : (Float -> Quantity Float units) -> ( Float, Float, Float ) -> Point3d units coordinates
fromTuple toQuantity ( x, y, z ) =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Point3d` to a tuple of `Float` values, by specifying what units you want the result
to be in.

    point =
        Point3d.fromCoordinates
            (Length.feet 2)
            (Length.feet 3)
            (Length.feet 1)

    Point3d.toTuple Length.inInches point
    --> ( 24, 36, 12 )

-}
toTuple : (Quantity Float units -> Float) -> Point3d units coordinates -> ( Float, Float, Float )
toTuple fromQuantity point =
    ( fromQuantity (xCoordinate point)
    , fromQuantity (yCoordinate point)
    , fromQuantity (zCoordinate point)
    )


{-| Construct a `Point3d` from a record with `Float` fields, by specifying what units those fields
are in.

    Point3d.fromRecord Length.inches { x = 24, y = 36, z = 12 }
    --> Point3d.fromCoordinates
    -->     (Length.feet 2)
    -->     (Length.feet 3)
    -->     (Length.feet 1)

-}
fromRecord : (Float -> Quantity Float units) -> { x : Float, y : Float, z : Float } -> Point3d units coordinates
fromRecord toQuantity { x, y, z } =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Point3d` to a record with `Float` fields, by specifying what units you want the
result to be in.

    point =
        Point3d.fromCoordinates
            (Length.meters 2)
            (Length.meters 3)
            (Length.meters 1)

    Point3d.toRecord Length.inCentimeters point
    --> { x = 200, y = 300, z = 100 }

-}
toRecord : (Quantity Float units -> Float) -> Point3d units coordinates -> { x : Float, y : Float, z : Float }
toRecord fromQuantity point =
    { x = fromQuantity (xCoordinate point)
    , y = fromQuantity (yCoordinate point)
    , z = fromQuantity (zCoordinate point)
    }


{-| TODO
-}
xCoordinateIn : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> Quantity Float units
xCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.xDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| TODO
-}
yCoordinateIn : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> Quantity Float units
yCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.yDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| TODO
-}
zCoordinateIn : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> Quantity Float units
zCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.zDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Get the X coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.xCoordinate
    --> 2

-}
xCoordinate : Point3d units coordinates -> Quantity Float units
xCoordinate (Types.Point3d p) =
    Quantity p.x


{-| Get the Y coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.yCoordinate
    --> 1

-}
yCoordinate : Point3d units coordinates -> Quantity Float units
yCoordinate (Types.Point3d p) =
    Quantity p.y


{-| Get the Z coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.zCoordinate
    --> 3

-}
zCoordinate : Point3d units coordinates -> Quantity Float units
zCoordinate (Types.Point3d p) =
    Quantity p.z


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point3d.fromCoordinates ( 2, 1, 3 )

    secondPoint =
        Point3d.fromCoordinates ( 2.0002, 0.9999, 3.0001 )

    Point3d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point3d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Quantity Float units -> Point3d units coordinates -> Point3d units coordinates -> Bool
equalWithin (Quantity eps) (Types.Point3d p1) (Types.Point3d p2) =
    if eps > 0 then
        let
            nx =
                (p2.x - p1.x) / eps

            ny =
                (p2.y - p1.y) / eps

            nz =
                (p2.z - p1.z) / eps
        in
        nx * nx + ny * ny + nz * nz <= 1

    else if eps == 0 then
        p1.x == p2.x && p1.y == p2.y && p1.z == p2.z

    else
        False


{-| Compare two `Point3d` values lexicographically: first by X coordinate, then
by Y, then by Z. Can be used to provide a sort order for `Point3d` values.
-}
lexicographicComparison : Point3d units coordinates -> Point3d units coordinates -> Order
lexicographicComparison (Types.Point3d p1) (Types.Point3d p2) =
    if p1.x /= p2.x then
        compare p1.x p2.x

    else if p1.y /= p2.y then
        compare p1.y p2.y

    else
        compare p1.z p2.z


{-| Find the distance from the first point to the second.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 2 )

    p2 =
        Point3d.fromCoordinates ( 2, 3, 4 )

    Point3d.distanceFrom p1 p2
    --> 3

Partial application can be useful:

    points =
        [ Point3d.fromCoordinates ( 3, 4, 5 )
        , Point3d.fromCoordinates ( 10, 10, 10 )
        , Point3d.fromCoordinates ( -1, 2, -3 )
        ]

    points
        |> List.sortBy
            (Point3d.distanceFrom Point3d.origin)
    --> [ Point3d.fromCoordinates ( -1, 2, -3 )
    --> , Point3d.fromCoordinates ( 3, 4, 5 )
    --> , Point3d.fromCoordinates ( 10, 10, 10 )
    --> ]

-}
distanceFrom : Point3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFrom (Types.Point3d p1) (Types.Point3d p2) =
    let
        deltaX =
            p2.x - p1.x

        deltaY =
            p2.y - p1.y

        deltaZ =
            p2.z - p1.z

        largestComponent =
            max (abs deltaX) (max (abs deltaY) (abs deltaZ))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                deltaX / largestComponent

            scaledY =
                deltaY / largestComponent

            scaledZ =
                deltaZ / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Quantity (scaledLength * largestComponent)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d.withDirection Direction3d.x
            (Point3d.fromCoordinates ( 1, 0, 0 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceAlong axis point
    --> 2

    Point3d.signedDistanceAlong axis Point3d.origin
    --> -1

-}
signedDistanceAlong : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceAlong (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Find the perpendicular (nearest) distance of a point from an axis.

    point =
        Point3d.fromCoordinates ( -3, 4, 0 )

    Point3d.distanceFromAxis Axis3d.x point
    --> 4

    Point3d.distanceFromAxis Axis3d.y point
    --> 3

    Point3d.distanceFromAxis Axis3d.z point
    --> 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.

-}
distanceFromAxis : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFromAxis (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z

        projection =
            deltaX * d.x + deltaY * d.y + deltaZ * d.z

        perpX =
            deltaX - projection * d.x

        perpY =
            deltaY - projection * d.y

        perpZ =
            deltaZ - projection * d.z

        largestComponent =
            max (abs perpX) (max (abs perpY) (abs perpZ))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                perpX / largestComponent

            scaledY =
                perpY / largestComponent

            scaledZ =
                perpZ / largestComponent

            scaledDistance =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Quantity (scaledDistance * largestComponent)


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane.

    plane =
        Plane3d.withNormalDirection Direction3d.y
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceFrom plane point
    --> 1

    Point3d.signedDistanceFrom plane Point3d.origin
    --> -2

This means that flipping a plane (reversing its normal direction) will also flip
the sign of the result of this function:

    flippedPlane =
        Plane3d.reverseNormal plane

    Point3d.signedDistanceFrom flippedPlane point
    --> -1

    Point3d.signedDistanceFrom flippedPlane Point3d.origin
    --> 2

-}
signedDistanceFrom : Plane3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceFrom (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection
    in
    Quantity ((p.x - p0.x) * n.x + (p.y - p0.y) * n.y + (p.z - p0.z) * n.z)


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point3d.fromCoordinates ( 1, 1, 1 )

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.scaleAbout centerPoint 3 point
    --> Point3d.fromCoordinates ( 1, 4, 7 )

    Point3d.scaleAbout centerPoint 0.5 point
    --> Point3d.fromCoordinates ( 1, 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point3d units coordinates -> Float -> Point3d units coordinates -> Point3d units coordinates
scaleAbout (Types.Point3d p0) k (Types.Point3d p) =
    Types.Point3d
        { x = p0.x + k * (p.x - p0.x)
        , y = p0.y + k * (p.y - p0.y)
        , z = p0.z + k * (p.z - p0.z)
        }


{-| Rotate a point around an axis by a given angle (in radians).

    axis =
        Axis3d.x

    angle =
        degrees 45

    point =
        Point3d.fromCoordinates ( 3, 1, 0 )

    Point3d.rotateAround axis angle point
    --> Point3d.fromCoordinates ( 3, 0.7071, 0.7071 )

Rotation direction is given by the right-hand rule, counterclockwise around the
direction of the axis.

-}
rotateAround : Axis3d units coordinates -> Angle -> Point3d units coordinates -> Point3d units coordinates
rotateAround (Types.Axis3d axis) (Quantity angle) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        qx =
            d.x * sinHalfAngle

        qy =
            d.y * sinHalfAngle

        qz =
            d.z * sinHalfAngle

        qw =
            cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point3d
        { x = p0.x + a00 * deltaX + a01 * deltaY + a02 * deltaZ
        , y = p0.y + a10 * deltaX + a11 * deltaY + a12 * deltaZ
        , z = p0.z + a20 * deltaX + a21 * deltaY + a22 * deltaZ
        }


{-| Translate a point by a given displacement.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    Point3d.translateBy displacement point
    --> Point3d.fromCoordinates ( 4, 6, 8 )

In more mathematical terms, this is 'point plus vector'. For 'point minus point'
(giving the vector from one point to another), there is [`Vector3d.from`](Vector3d#from).

-}
translateBy : Vector3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
translateBy (Types.Vector3d v) (Types.Point3d p) =
    Types.Point3d
        { x = p.x + v.x
        , y = p.y + v.y
        , z = p.z + v.z
        }


{-| Translate a point in a given direction by a given distance.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    point |> Point3d.translateIn Direction3d.x 2
    --> Point3d.fromCoordinates ( 5, 4, 5 )

    point |> Point3d.translateIn Direction3d.y 2
    --> Point3d.fromCoordinates ( 3, 6, 5 )

The distance can be negative:

    Point3d.translateIn Direction3d.x -2
    --> Point3d.fromCoordinates ( 1, 4, 5 )

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Point3d units coordinates -> Point3d units coordinates
translateIn (Types.Direction3d d) (Quantity distance) (Types.Point3d p) =
    Types.Point3d
        { x = p.x + distance * d.x
        , y = p.y + distance * d.y
        , z = p.z + distance * d.z
        }


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    -- Plane3d.xy is the plane Z=0
    Point3d.mirrorAcross Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, -3 )

    -- Plane3d.yz is the plane X=0
    Point3d.mirrorAcross Plane3d.yz point
    --> Point3d.fromCoordinates ( -1, 2, 3 )

The plane does not have to pass through the origin:

    -- offsetPlane is the plane  Z=1
    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 2 )

-}
mirrorAcross : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
mirrorAcross (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection

        a00 =
            1 - 2 * n.x * n.x

        a11 =
            1 - 2 * n.y * n.y

        a22 =
            1 - 2 * n.z * n.z

        a12 =
            -2 * n.y * n.z

        a02 =
            -2 * n.x * n.z

        a01 =
            -2 * n.x * n.y

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point3d
        { x = p0.x + a00 * deltaX + a01 * deltaY + a02 * deltaZ
        , y = p0.y + a01 * deltaX + a11 * deltaY + a12 * deltaZ
        , z = p0.z + a02 * deltaX + a12 * deltaY + a22 * deltaZ
        }


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a point onto a plane:

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOnto Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, 0 )

    Point3d.projectOnto Plane3d.yz point
    --> Point3d.fromCoordinates ( 0, 2, 3 )

The plane does not have to pass through the origin:

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane point
    --> Point3d.fromCoordinates ( 1, 2, 1 )

-}
projectOnto : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOnto (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection

        distance =
            (p.x - p0.x) * n.x + (p.y - p0.y) * n.y + (p.z - p0.z) * n.z
    in
    Types.Point3d
        { x = p.x - distance * n.x
        , y = p.y - distance * n.y
        , z = p.z - distance * n.z
        }


{-| Project a point perpendicularly onto an axis.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOntoAxis Axis3d.x
    --> Point3d.fromCoordinates ( 1, 0, 0 )

    verticalAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.fromCoordinates ( 0, 1, 2 ))

    Point3d.projectOntoAxis verticalAxis
    --> Point3d.fromCoordinates ( 0, 1, 3 )

-}
projectOntoAxis : Axis3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOntoAxis (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        distance =
            (p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z
    in
    Types.Point3d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        , z = p0.z + distance * d.z
        }


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 4, 5, 6 ))
    --> Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 1, 1, 1 ))
    --> Point3d.fromCoordinates ( 0, -1, -2 )

-}
relativeTo : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> Point3d units localCoordinates
relativeTo (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.x
    in
    Types.Point3d
        { x = deltaX * i.x + deltaY * i.y + deltaZ * i.z
        , y = deltaX * j.x + deltaY * j.y + deltaZ * j.z
        , z = deltaX * k.x + deltaY * k.y + deltaZ * k.z
        }


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 3, 3, 3 ))
    --> Point3d.fromCoordinates ( 4, 5, 6 )

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 0, -1, -2 ))
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
placeIn : Frame3d units globalCoordinates localCoordinates -> Point3d units localCoordinates -> Point3d units globalCoordinates
placeIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Point3d
        { x = p0.x + p.x * i.x + p.y * j.x + p.z * k.x
        , y = p0.y + p.x * i.y + p.y * j.y + p.z * k.y
        , z = p0.z + p.x * i.z + p.y * j.z + p.z * k.z
        }


{-| Project a point into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the point onto the plane and then expresses the projected point in 2D sketch
coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    Point3d.projectInto SketchPlane3d.xy point
    --> Point2d.fromCoordinates ( 2, 1 )

    Point3d.projectInto SketchPlane3d.yz point
    --> Point2d.fromCoordinates ( 1, 3 )

    Point3d.projectInto SketchPlane3d.zx point
    --> Point2d.fromCoordinates ( 3, 2 )

-}
projectInto : SketchPlane3d units coordinates coordinates2d -> Point3d units coordinates -> Point2d units coordinates2d
projectInto (Types.SketchPlane3d sketchPlane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point2d
        { x = deltaX * i.x + deltaY * i.y + deltaZ * i.z
        , y = deltaX * j.x + deltaY * j.y + deltaZ * j.z
        }
