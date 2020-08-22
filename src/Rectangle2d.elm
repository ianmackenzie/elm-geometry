--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Rectangle2d exposing
    ( Rectangle2d
    , with, from, withDimensions, centeredOn, withXAxis, withYAxis, fromBoundingBox
    , dimensions, axes, xAxis, yAxis, centerPoint, area, vertices, edges, boundingBox
    , interpolate
    , contains
    , toPolygon
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , randomPoint
    )

{-| A `Rectangle2d` represents a rectangle in 2D space. This module contains
rectangle-related functionality such as:

  - Constructing rectangles in various ways
  - Extracting rectangle vertices and edges
  - Scaling, rotating, translating and mirroring rectangles
  - Converting rectangles between different coordinate systems

Unlike bounding boxes, rectangles are _not_ constrained to be axis-aligned -
they can have arbitrary orientation and so can be rotated, mirrored etc.

@docs Rectangle2d


# Construction

@docs with, from, withDimensions, centeredOn, withXAxis, withYAxis, fromBoundingBox


# Properties

@docs dimensions, axes, xAxis, yAxis, centerPoint, area, vertices, edges, boundingBox


# Interpolation

@docs interpolate


# Querying

@docs contains


# Conversion

@docs toPolygon


# Transformation

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Random point generation

@docs randomPoint

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate, Squared)
import Random exposing (Generator)
import Vector2d exposing (Vector2d)


{-| -}
type alias Rectangle2d units coordinates =
    Types.Rectangle2d units coordinates


{-| Construct an axis-aligned rectangle given the X and Y coordinates of two
diagonally opposite vertices. The X and Y directions of the resulting rectangle
are determined by the order of the given values:

  - If `x1 <= x2`, then the rectangle's X direction will be
    `Direction2d.positiveX`, otherwise it will be `Direction2d.negativeX`
  - If `y1 <= y2`, then the rectangle's Y direction will be
    `Direction2d.positiveY`, otherwise it will be `Direction2d.negativeY`

Therefore, something like

    Rectangle2d.with
        { x1 = Pixels.pixels 0
        , y1 = Pixels.pixels 300
        , x2 = Pixels.pixels 500
        , y2 = Pixels.pixels 0
        }

would have its X direction equal to `Direction2d.positiveX` and its Y direction
equal to `Direction2d.negativeY`.

-}
with : { x1 : Quantity Float units, y1 : Quantity Float units, x2 : Quantity Float units, y2 : Quantity Float units } -> Rectangle2d units coordinates
with { x1, y1, x2, y2 } =
    axisAligned x1 y1 x2 y2


{-| Construct an axis-aligned rectangle stretching from one point to another;

    Rectangle2d.from p1 p2

is equivalent to

    Rectangle2d.with
        { x1 = Point2d.xCoordinate p1
        , y1 = Point2d.yCoordinate p1
        , x2 = Point2d.xCoordinate p2
        , y2 = Point2d.yCoordinate p2
        }

and so the same logic about the resulting rectangle's X and Y directions
applies (see above for details). Therefore, assuming a Y-up coordinate system,
something like

    Rectangle2d.from lowerLeftPoint upperRightPoint

would have positive X and Y directions, while

    Rectangle2d.from upperLeftPoint lowerRightPoint

would have a positive X direction but a negative Y direction.

-}
from : Point2d units coordinates -> Point2d units coordinates -> Rectangle2d units coordinates
from p1 p2 =
    axisAligned
        (Point2d.xCoordinate p1)
        (Point2d.yCoordinate p1)
        (Point2d.xCoordinate p2)
        (Point2d.yCoordinate p2)


{-| Construct a rectangle with the given overall dimensions (width/height) and
angle, centered on the given point. For an axis-aligned rectangle you can pass
`Angle.degrees 0` (or equivalently `Angle.radians 0` or even `Quantity.zero`) as
the second argument:

    rectangle =
        Rectangle2d.withDimensions
            ( Pixels.pixels 200, Pixels.pixels 100 )
            (Angle.degrees 0)
            (Point2d.pixels 400 300)

    Rectangle2d.vertices rectangle
    --> [ Point2d.pixels 300 250
    --> , Point2d.pixels 500 250
    --> , Point2d.pixels 500 350
    --> , Point2d.pixels 300 350
    --> ]

Passing a non-zero angle lets you control the orientation of a rectangle; to
make a diamond shape you might do something like

    diamond =
        Rectangle2d.withDimensions
            ( Length.meters 2, Length.meters 2 )
            (Angle.degrees 45)
            Point2d.origin

    Rectangle2d.vertices diamond
    --> [ Point2d.meters 0 -1.4142
    --> , Point2d.meters 1.4142 0
    --> , Point2d.meters 0 1.4142
    --> , Point2d.meters -1.4142 0
    --> ]

-}
withDimensions : ( Quantity Float units, Quantity Float units ) -> Angle -> Point2d units coordinates -> Rectangle2d units coordinates
withDimensions ( givenWidth, givenHeight ) givenAngle givenCenter =
    Types.Rectangle2d
        { axes = Frame2d.withAngle givenAngle givenCenter
        , dimensions = ( Quantity.abs givenWidth, Quantity.abs givenHeight )
        }


{-| Construct a frame centered on the given axes, with the given overall
dimensions;

    Rectangle2d.withDimensions dimensions angle centerPoint

could be written as

    Rectangle2d.centeredOn
        (Frame2d.withAngle angle centerPoint)
        dimensions

-}
centeredOn : Frame2d units coordinates defines -> ( Quantity Float units, Quantity Float units ) -> Rectangle2d units coordinates
centeredOn givenAxes ( givenWidth, givenHeight ) =
    Types.Rectangle2d
        { axes = Frame2d.copy givenAxes
        , dimensions = ( Quantity.abs givenWidth, Quantity.abs givenHeight )
        }


{-| Construct a rectangle with the given X axis and overall dimensions. The
rectangle will be centered on the axis' origin point.
-}
withXAxis : Axis2d units coordinates -> ( Quantity Float units, Quantity Float units ) -> Rectangle2d units coordinates
withXAxis givenAxis givenDimensions =
    centeredOn (Frame2d.fromXAxis givenAxis) givenDimensions


{-| Construct a rectangle with the given Y axis and overall dimensions. The
rectangle will be centered on the axis' origin point.
-}
withYAxis : Axis2d units coordinates -> ( Quantity Float units, Quantity Float units ) -> Rectangle2d units coordinates
withYAxis givenAxis givenDimensions =
    centeredOn (Frame2d.fromYAxis givenAxis) givenDimensions


{-| Convert a `BoundingBox2d` to the equivalent axis-aligned `Rectangle2d`.
-}
fromBoundingBox : BoundingBox2d units coordinates -> Rectangle2d units coordinates
fromBoundingBox givenBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema givenBox
    in
    axisAligned minX minY maxX maxY


axisAligned : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Rectangle2d units coordinates
axisAligned x1 y1 x2 y2 =
    let
        computedCenterPoint =
            Point2d.xy (Quantity.midpoint x1 x2) (Quantity.midpoint y1 y2)

        computedXDirection =
            if x2 |> Quantity.greaterThanOrEqualTo x1 then
                Direction2d.positiveX

            else
                Direction2d.negativeX

        computedYDirection =
            if y2 |> Quantity.greaterThanOrEqualTo y1 then
                Direction2d.positiveY

            else
                Direction2d.negativeY

        computedAxes =
            Frame2d.unsafe
                { originPoint = computedCenterPoint
                , xDirection = computedXDirection
                , yDirection = computedYDirection
                }

        computedDimensions =
            ( Quantity.abs (x2 |> Quantity.minus x1)
            , Quantity.abs (y2 |> Quantity.minus y1)
            )
    in
    Types.Rectangle2d
        { axes = computedAxes
        , dimensions = computedDimensions
        }


{-| Convert a rectangle from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Rectangle2d units1 coordinates -> Rectangle2d units2 coordinates
at rate (Types.Rectangle2d rectangle) =
    let
        ( width, height ) =
            rectangle.dimensions
    in
    Types.Rectangle2d
        { axes = Frame2d.at rate rectangle.axes
        , dimensions =
            ( Quantity.abs (Quantity.at rate width)
            , Quantity.abs (Quantity.at rate height)
            )
        }


{-| Convert a rectangle from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Rectangle2d units1 coordinates -> Rectangle2d units2 coordinates
at_ rate rectangle =
    at (Quantity.inverse rate) rectangle


{-| Convert a rectangle to a [`Polygon2d`](Polygon2d#Polygon2d).
-}
toPolygon : Rectangle2d units coordinates -> Polygon2d units coordinates
toPolygon rectangle =
    Polygon2d.singleLoop (vertices rectangle)


{-| Get the central axes of a rectangle as a `Frame2d`:

    rectangle =
        Rectangle2d.with
            { x1 = Length.meters 2
            , x2 = Length.meters 5
            , y1 = Length.meters 1
            , y2 = Length.meters 3
            }

    Rectangle2d.axes rectangle
    --> Frame2d.atPoint (Point2d.meters 3.5 2)

The origin point of the frame will be the center point of the rectangle.

-}
axes : Rectangle2d units coordinates -> Frame2d units coordinates defines
axes (Types.Rectangle2d rectangle) =
    Frame2d.copy rectangle.axes


{-| Get the X axis of a rectangle.
-}
xAxis : Rectangle2d units coordinates -> Axis2d units coordinates
xAxis rectangle =
    Frame2d.xAxis (axes rectangle)


{-| Get the Y axis of a rectangle.
-}
yAxis : Rectangle2d units coordinates -> Axis2d units coordinates
yAxis rectangle =
    Frame2d.yAxis (axes rectangle)


{-| Get the center point of a rectangle.
-}
centerPoint : Rectangle2d units coordinates -> Point2d units coordinates
centerPoint rectangle =
    Frame2d.originPoint (axes rectangle)


{-| Get the overall dimensions (width and height) of a rectangle:

    rectangle =
        Rectangle2d.with
            { x1 = Length.meters 2
            , x2 = Length.meters 5
            , y1 = Length.meters 1
            , y2 = Length.meters 3
            }

    Rectangle2d.dimensions rectangle
    --> ( Length.meters 3, Length.meters 2 )

-}
dimensions : Rectangle2d units coordinates -> ( Quantity Float units, Quantity Float units )
dimensions (Types.Rectangle2d rectangle) =
    rectangle.dimensions


{-| Get the area of a rectangle.
-}
area : Rectangle2d units coordinates -> Quantity Float (Squared units)
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width |> Quantity.times height


{-| Get the vertices of a rectangle as a list. The vertices will be returned
in counterclockwise order if the rectangle's axes are right-handed, and
clockwise order if the axes are left-handed.
-}
vertices : Rectangle2d units coordinates -> List (Point2d units coordinates)
vertices rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        x =
            Quantity.half width

        y =
            Quantity.half height
    in
    [ Point2d.xyIn localFrame (Quantity.negate x) (Quantity.negate y)
    , Point2d.xyIn localFrame x (Quantity.negate y)
    , Point2d.xyIn localFrame x y
    , Point2d.xyIn localFrame (Quantity.negate x) y
    ]


{-| Check if a rectangle contains a given point.
-}
contains : Point2d units coordinates -> Rectangle2d units coordinates -> Bool
contains point rectangle =
    let
        ( width, height ) =
            dimensions rectangle

        localFrame =
            axes rectangle

        x =
            Point2d.xCoordinateIn localFrame point

        y =
            Point2d.yCoordinateIn localFrame point
    in
    (Quantity.abs x |> Quantity.lessThanOrEqualTo (Quantity.half width))
        && (Quantity.abs y |> Quantity.lessThanOrEqualTo (Quantity.half height))


{-| Get the edges of a rectangle as a list. The edges will be returned
in counterclockwise order if the rectangle's axes are right-handed, and
clockwise order if the axes are left-handed.
-}
edges : Rectangle2d units coordinates -> List (LineSegment2d units coordinates)
edges rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        x =
            Quantity.half width

        y =
            Quantity.half height

        p1 =
            Point2d.xyIn localFrame (Quantity.negate x) (Quantity.negate y)

        p2 =
            Point2d.xyIn localFrame x (Quantity.negate y)

        p3 =
            Point2d.xyIn localFrame x y

        p4 =
            Point2d.xyIn localFrame (Quantity.negate x) y
    in
    [ LineSegment2d.from p1 p2
    , LineSegment2d.from p2 p3
    , LineSegment2d.from p3 p4
    , LineSegment2d.from p4 p1
    ]


{-| Scale a rectangle about a given point by a given scale. Note that scaling by
a negative value will flip the handedness of the rectangle's axes, and therefore
the order/direction of results from `Rectangle2d.vertices` and
`Rectangle2d.edges` will change.
-}
scaleAbout : Point2d units coordinates -> Float -> Rectangle2d units coordinates -> Rectangle2d units coordinates
scaleAbout point scale rectangle =
    let
        currentFrame =
            axes rectangle

        currentXDirection =
            Frame2d.xDirection currentFrame

        currentYDirection =
            Frame2d.yDirection currentFrame

        newCenterPoint =
            Point2d.scaleAbout point scale (Frame2d.originPoint currentFrame)

        newAxes =
            if scale >= 0 then
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    }

            else
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction2d.reverse currentXDirection
                    , yDirection = Direction2d.reverse currentYDirection
                    }

        ( currentWidth, currentHeight ) =
            dimensions rectangle

        newWidth =
            Quantity.abs (Quantity.multiplyBy scale currentWidth)

        newHeight =
            Quantity.abs (Quantity.multiplyBy scale currentHeight)
    in
    Types.Rectangle2d
        { axes = newAxes
        , dimensions = ( newWidth, newHeight )
        }


{-| Rotate a rectangle around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Rectangle2d units coordinates -> Rectangle2d units coordinates
rotateAround point angle rectangle =
    Types.Rectangle2d
        { axes = Frame2d.rotateAround point angle (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle by a given displacement.
-}
translateBy : Vector2d units coordinates -> Rectangle2d units coordinates -> Rectangle2d units coordinates
translateBy displacement rectangle =
    Types.Rectangle2d
        { axes = Frame2d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Rectangle2d units coordinates -> Rectangle2d units coordinates
translateIn direction distance rectangle =
    translateBy (Vector2d.withLength distance direction) rectangle


{-| Mirror a rectangle across a given axis. Note that this will flip the
handedness of the rectangle's axes, and therefore the order/direction of results
from `Rectangle2d.vertices` and `Rectangle2d.edges` will change.
-}
mirrorAcross : Axis2d units coordinates -> Rectangle2d units coordinates -> Rectangle2d units coordinates
mirrorAcross axis rectangle =
    Types.Rectangle2d
        { axes = Frame2d.mirrorAcross axis (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Take a rectangle considered to be defined in local coordinates relative to a
given reference frame, and return that rectangle expressed in global
coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Rectangle2d units localCoordinates -> Rectangle2d units globalCoordinates
placeIn frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Take a rectangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Rectangle2d units globalCoordinates -> Rectangle2d units localCoordinates
relativeTo frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Get the minimal bounding box containing a given rectangle. This will have
exactly the same shape and size as the rectangle itself if the rectangle is
axis-aligned, but will be larger than the rectangle if the rectangle is at an
angle.

    square =
        Rectangle2d.with
            { x1 = Length.meters 0
            , x2 = Length.meters 1
            , y1 = Length.meters 0
            , y2 = Length.meters 1
            }

    diamond =
        square
            |> Rectangle2d.rotateAround Point2d.origin
                (Angle.degrees 45)

    Rectangle2d.boundingBox diamond
    --> BoundingBox2d.from
    -->     (Point2d.meters -0.7071 0)
    -->     (Point2d.meters 0.7071 1.4142)

-}
boundingBox : Rectangle2d units coordinates -> BoundingBox2d units coordinates
boundingBox rectangle =
    let
        frame =
            axes rectangle

        p0 =
            Point2d.unwrap (Frame2d.originPoint frame)

        i =
            Direction2d.unwrap (Frame2d.xDirection frame)

        ix =
            abs i.x

        iy =
            abs i.y

        j =
            Direction2d.unwrap (Frame2d.yDirection frame)

        jx =
            abs j.x

        jy =
            abs j.y

        ( Quantity width, Quantity height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2

        minX =
            p0.x - ix * halfWidth - jx * halfHeight

        maxX =
            p0.x + ix * halfWidth + jx * halfHeight

        minY =
            p0.y - iy * halfWidth - jy * halfHeight

        maxY =
            p0.y + iy * halfWidth + jy * halfHeight
    in
    Types.BoundingBox2d
        { minX = Quantity minX
        , maxX = Quantity maxX
        , minY = Quantity minY
        , maxY = Quantity maxY
        }


{-| Interpolate within a rectangle based on coordinates which range from 0 to 1.
For example, the four vertices of a given rectangle are

    [ Rectangle2d.interpolate rectangle 0 0
    , Rectangle2d.interpolate rectangle 1 0
    , Rectangle2d.interpolate rectangle 1 1
    , Rectangle2d.interpolate rectangle 0 1
    ]

and its center point is

    Rectangle2d.interpolate rectangle 0.5 0.5

-}
interpolate : Rectangle2d units coordinates -> Float -> Float -> Point2d units coordinates
interpolate rectangle u v =
    let
        ( width, height ) =
            dimensions rectangle
    in
    Point2d.xyIn (axes rectangle)
        (Quantity.multiplyBy (u - 0.5) width)
        (Quantity.multiplyBy (v - 0.5) height)


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for points within a given rectangle.
-}
randomPoint : Rectangle2d units coordinates -> Generator (Point2d units coordinates)
randomPoint rectangle =
    Random.map2 (interpolate rectangle) (Random.float 0 1) (Random.float 0 1)
