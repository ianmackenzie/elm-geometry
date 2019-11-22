--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Circle2d exposing
    ( Circle2d
    , atPoint, withRadius, atOrigin, throughPoints, sweptAround
    , centerPoint, radius, diameter, area, circumference, boundingBox
    , toArc
    , contains, intersectsBoundingBox
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Circle2d` is defined by its center point and radius. This module includes
functionality for

  - Constructing circles through points or with a given center/radius
  - Scaling, rotating and translating circles
  - Extracting properties of circles like area, center point and radius

@docs Circle2d


# Constructors

@docs atPoint, withRadius, atOrigin, throughPoints, sweptAround


# Properties

@docs centerPoint, radius, diameter, area, circumference, boundingBox


# Conversion

@docs toArc


# Queries

@docs contains, intersectsBoundingBox


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (Arc2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias Circle2d units coordinates =
    Types.Circle2d units coordinates


{-| Construct a circle from its radius and center point:

    exampleCircle =
        Circle2d.atPoint (Point2d.meters 1 2)
            (Length.meters 3)

If you pass a negative radius, the absolute value will be used.

-}
atPoint : Point2d units coordinates -> Quantity Float units -> Circle2d units coordinates
atPoint givenCenterPoint givenRadius =
    withRadius givenRadius givenCenterPoint


{-| Construct a circle from its radius and center point. Flipped version of
`atPoint` that may be more useful in some situations like constructing a bunch
of circles of the same radius at different points:

    circles =
        List.map (Circle2d.withRadius radius) listOfPoints

-}
withRadius : Quantity Float units -> Point2d units coordinates -> Circle2d units coordinates
withRadius givenRadius givenCenterPoint =
    Types.Circle2d
        { radius = Quantity.abs givenRadius
        , centerPoint = givenCenterPoint
        }


{-| Construct a circle at the origin with the given radius.
-}
atOrigin : Quantity Float units -> Circle2d units coordinates
atOrigin givenRadius =
    atPoint Point2d.origin givenRadius


{-| Attempt to construct a circle that passes through the three given points:

    Circle2d.throughPoints
        Point2d.origin
        (Point2d.meters 1 0)
        (Point2d.meters 0 1)
    --> Just <|
    -->     Circle2d.atPoint (Point2d.meters 0.5 0.5)
    -->         (Length.meters 0.7071)

If the three given points are collinear, returns `Nothing`:

    Circle2d.throughPoints
        Point2d.origin
        (Point2d.meters 2 0)
        (Point2d.meters 4 0)
    --> Nothing

-}
throughPoints : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Circle2d units coordinates)
throughPoints p1 p2 p3 =
    Point2d.circumcenter p1 p2 p3
        |> Maybe.map
            (\p0 ->
                let
                    r1 =
                        Point2d.distanceFrom p0 p1

                    r2 =
                        Point2d.distanceFrom p0 p2

                    r3 =
                        Point2d.distanceFrom p0 p3

                    r =
                        Quantity.multiplyBy (1 / 3)
                            (r1 |> Quantity.plus r2 |> Quantity.plus r3)
                in
                withRadius r p0
            )


{-| Construct a circle by rotating a point on the circle around a given center
point. The center point is given first and the point on the circle is given
second.

    Circle2d.sweptAround Point2d.origin
        (Point2d.meters 2 0)
    --> Circle2d.atOrigin (Length.meters 2)

The above example could be rewritten as

    Point2d.meters 2 0
        |> Circle2d.sweptAround Point2d.origin

and if you wanted to create many concentric circles all centered on a specific
point but passing through several other different points, you could use
something like

    concentricCircles =
        List.map (Circle2d.sweptAround centerPoint) points

-}
sweptAround : Point2d units coordinates -> Point2d units coordinates -> Circle2d units coordinates
sweptAround centerPoint_ point =
    withRadius (Point2d.distanceFrom centerPoint_ point) centerPoint_


{-| Convert a circle from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Circle2d units1 coordinates -> Circle2d units2 coordinates
at rate (Types.Circle2d circle) =
    Types.Circle2d
        { centerPoint = Point2d.at rate circle.centerPoint
        , radius = Quantity.abs (Quantity.at rate circle.radius)
        }


{-| Convert a circle from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Circle2d units1 coordinates -> Circle2d units2 coordinates
at_ rate circle =
    at (Quantity.inverse rate) circle


{-| Get the center point of a circle.
-}
centerPoint : Circle2d units coordinates -> Point2d units coordinates
centerPoint (Types.Circle2d properties) =
    properties.centerPoint


{-| Get the radius of a circle.
-}
radius : Circle2d units coordinates -> Quantity Float units
radius (Types.Circle2d properties) =
    properties.radius


{-| Get the diameter of a circle (twice the radius).
-}
diameter : Circle2d units coordinates -> Quantity Float units
diameter circle =
    Quantity.multiplyBy 2 (radius circle)


{-| Get the area of a circle.
-}
area : Circle2d units coordinates -> Quantity Float (Squared units)
area circle =
    Quantity.multiplyBy pi (Quantity.squared (radius circle))


{-| Get the circumference (perimeter) of a circle.
-}
circumference : Circle2d units coordinates -> Quantity Float units
circumference circle =
    Quantity.multiplyBy (2 * pi) (radius circle)


{-| Convert a circle to a 360 degree arc.
-}
toArc : Circle2d units coordinates -> Arc2d units coordinates
toArc (Types.Circle2d circle) =
    let
        startX =
            Point2d.xCoordinate circle.centerPoint |> Quantity.plus circle.radius

        startY =
            Point2d.yCoordinate circle.centerPoint
    in
    Types.Arc2d
        { startPoint = Point2d.xy startX startY
        , xDirection = Direction2d.y
        , sweptAngle = Angle.radians (2 * pi)
        , signedLength = Quantity.multiplyBy (2 * pi) circle.radius
        }


{-| Check if a circle contains a given point.
-}
contains : Point2d units coordinates -> Circle2d units coordinates -> Bool
contains point circle =
    point |> Point2d.distanceFrom (centerPoint circle) |> Quantity.lessThanOrEqualTo (radius circle)


{-| Check if a circle intersects with a given bounding box. This function will
return true if the circle intersects the edges of the bounding box _or_ is fully
contained within the bounding box.

    boundingBox =
        BoundingBox2d.from
            (Point2d.meters 2 0)
            (Point2d.meters 3 2)

    circle =
        Circle2d.atOrigin (Length.meters 3)

    Circle2d.intersectsBoundingBox boundingBox circle
    --> True

-}
intersectsBoundingBox : BoundingBox2d units coordinates -> Circle2d units coordinates -> Bool
intersectsBoundingBox box circle =
    let
        boxMinX =
            BoundingBox2d.minX box

        boxMaxX =
            BoundingBox2d.maxX box

        boxMinY =
            BoundingBox2d.minY box

        boxMaxY =
            BoundingBox2d.maxY box

        circleRadius =
            radius circle

        circleCenter =
            centerPoint circle

        circleX =
            Point2d.xCoordinate circleCenter

        circleY =
            Point2d.yCoordinate circleCenter

        deltaX =
            circleX
                |> Quantity.minus
                    (Quantity.max boxMinX (Quantity.min circleX boxMaxX))

        deltaY =
            circleY
                |> Quantity.minus
                    (Quantity.max boxMinY (Quantity.min circleY boxMaxY))
    in
    Quantity.squared deltaX
        |> Quantity.plus (Quantity.squared deltaY)
        |> Quantity.lessThanOrEqualTo (Quantity.squared circleRadius)


{-| Scale a circle about a given point by a given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> Circle2d units coordinates -> Circle2d units coordinates
scaleAbout point scale (Types.Circle2d circle) =
    withRadius (Quantity.multiplyBy (abs scale) circle.radius)
        (Point2d.scaleAbout point scale circle.centerPoint)


{-| Rotate a circle around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Circle2d units coordinates -> Circle2d units coordinates
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
    \(Types.Circle2d circle) ->
        withRadius circle.radius (rotatePoint circle.centerPoint)


{-| Translate a circle by a given displacement.
-}
translateBy : Vector2d units coordinates -> Circle2d units coordinates -> Circle2d units coordinates
translateBy displacement (Types.Circle2d circle) =
    withRadius circle.radius
        (Point2d.translateBy displacement circle.centerPoint)


{-| Translate a circle in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Circle2d units coordinates -> Circle2d units coordinates
translateIn direction distance circle =
    translateBy (Vector2d.withLength distance direction) circle


{-| Mirror a circle across a given axis.
-}
mirrorAcross : Axis2d units coordinates -> Circle2d units coordinates -> Circle2d units coordinates
mirrorAcross axis (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.mirrorAcross axis circle.centerPoint)


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Circle2d units globalCoordinates -> Circle2d units localCoordinates
relativeTo frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.relativeTo frame circle.centerPoint)


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Circle2d units localCoordinates -> Circle2d units globalCoordinates
placeIn frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.placeIn frame circle.centerPoint)


{-| Get the minimal bounding box containing a given circle.

    Circle2d.boundingBox exampleCircle
    --> BoundingBox2d.from
    -->     (Point2d.meters -2 -1)
    -->     (Point2d.meters 4 5)

-}
boundingBox : Circle2d units coordinates -> BoundingBox2d units coordinates
boundingBox circle =
    BoundingBox2d.fromExtrema
        { minX = Point2d.xCoordinate (centerPoint circle) |> Quantity.minus (radius circle)
        , maxX = Point2d.xCoordinate (centerPoint circle) |> Quantity.plus (radius circle)
        , minY = Point2d.yCoordinate (centerPoint circle) |> Quantity.minus (radius circle)
        , maxY = Point2d.yCoordinate (centerPoint circle) |> Quantity.plus (radius circle)
        }
