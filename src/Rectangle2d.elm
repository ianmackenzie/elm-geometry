--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Rectangle2d exposing
    ( Rectangle2d, RectangleCoordinates
    , from, centeredOn, fromExtrema, fromExtremaIn
    , dimensions, axes, xAxis, yAxis, centerPoint, area
    , vertices, bottomLeftVertex, bottomRightVertex, topLeftVertex, topRightVertex
    , edges, leftEdge, bottomEdge, rightEdge, topEdge
    , boundingBox
    , contains
    , toPolygon
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , relativeTo, placeIn
    )

{-| A `Rectangle2d` represents a rectangle in 2D space. This module contains
rectangle-related functionality such as:

  - Constructing rectangles in various ways
  - Extracting rectangle vertices and edges
  - Scaling, rotating, translating and mirroring rectangles
  - Converting rectangles between different coordinate systems

Unlike bounding boxes, rectangles are _not_ constrained to be axis-aligned -
they can have arbitrary orientation and so can be rotated, mirrored etc.

@docs Rectangle2d, RectangleCoordinates


# Construction

@docs from, centeredOn, fromExtrema, fromExtremaIn


# Properties

@docs dimensions, axes, xAxis, yAxis, centerPoint, area
@docs vertices, bottomLeftVertex, bottomRightVertex, topLeftVertex, topRightVertex
@docs edges, leftEdge, bottomEdge, rightEdge, topEdge
@docs boundingBox


# Querying

@docs contains


# Conversion

@docs toPolygon


# Transformation

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

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
import Quantity exposing (Quantity, Squared)
import Vector2d exposing (Vector2d)


{-| -}
type alias Rectangle2d units coordinates =
    Types.Rectangle2d units coordinates


{-| The coordinate system associated with the central axes of a rectangle.
-}
type alias RectangleCoordinates =
    Types.RectangleCoordinates


{-| Construct a rectangle centered on the given axes (frame), with the given
overall X/Y dimensions (width/height).

    frame =
        Frame2d.atCoordinates ( 3, 2 )

    rectangle =
        Rectangle2d.centeredOn frame ( 4, 3 )

    Rectangle2d.vertices rectangle
    --> { bottomLeft = Point2d.fromCoordinates ( 1, 0.5 )
    --> , bottomRight = Point2d.fromCoordinates ( 5, 0.5 )
    --> , topLeft = Point2d.fromCoordinates ( 1, 3.5 )
    --> , topRight = Point2d.fromCoordinates ( 5, 3.5 )
    --> }

-}
centeredOn : Frame2d units globalCoordinates localCoordinates -> ( Quantity Float units, Quantity Float units ) -> Rectangle2d units globalCoordinates
centeredOn givenAxes ( givenWidth, givenHeight ) =
    Types.Rectangle2d
        { axes = Frame2d.copy givenAxes
        , dimensions = ( Quantity.abs givenWidth, Quantity.abs givenHeight )
        }


{-| Construct a rectangle by supplying its maximum and minimum X and Y values
within a particular frame:

    frame =
        Frame2d.atCoordinates ( 5, 4 )

    Rectangle2d.fromExtremaIn frame
        { minX = -1
        , minY = -1
        , maxX = 3
        , maxY = 2
        }
    --> Rectangle2d.fromExtrema
    -->     { minX = 4
    -->     , minY = 3
    -->     , maxX = 8
    -->     , maxY = 6
    -->     }

Note that for simplicity we used a non-rotated frame in the above example - if
we had used a rotated frame, the result could not have been expressed using
`Rectangle2d.fromExtrema` since it would no longer have been axis-aligned.

-}
fromExtremaIn : Frame2d units globalCoordinates localCoordinates -> { minX : Quantity Float units, maxX : Quantity Float units, minY : Quantity Float units, maxY : Quantity Float units } -> Rectangle2d units globalCoordinates
fromExtremaIn localFrame { minX, maxX, minY, maxY } =
    let
        computedCenterPoint =
            Point2d.fromCoordinatesIn localFrame
                (Quantity.midpoint minX maxX)
                (Quantity.midpoint minY maxY)

        computedDimensions =
            ( Quantity.abs (maxX |> Quantity.minus minX)
            , Quantity.abs (maxY |> Quantity.minus minY)
            )
    in
    Types.Rectangle2d
        { axes = Frame2d.moveTo computedCenterPoint localFrame
        , dimensions = computedDimensions
        }


{-| Construct an axis-aligned rectangle stretching from one point to another.
The order of the points does not matter, and they can represent either the
lower left and upper right vertices or the upper left and lower right.

    p1 =
        Point2d.fromCoordinates ( 5, 2 )

    p2 =
        Point2d.fromCoordinates ( 1, 4 )

    Rectangle2d.from p1 p2
    --> Rectangle2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 5
    -->     , minY = 2
    -->     , maxY = 4
    -->     }

-}
from : Point2d units coordinates -> Point2d units coordinates -> Rectangle2d units coordinates
from firstPoint secondPoint =
    let
        computedAxes =
            Frame2d.atPoint (Point2d.midpoint firstPoint secondPoint)

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint

        computedDimensions =
            ( Quantity.abs (x2 |> Quantity.minus x1)
            , Quantity.abs (y2 |> Quantity.minus y1)
            )
    in
    Types.Rectangle2d
        { axes = computedAxes
        , dimensions = computedDimensions
        }


{-| Construct an axis-aligned rectangle by specifying its minimum and maximum
X and Y coordinates. If the min and max are given in the wrong order, they will
be swapped.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.vertices rectangle
    --> { bottomLeft = Point2d.fromCoordinates ( 2, 1 )
    --> , bottomRight = Point2d.fromCoordinates ( 5, 1 )
    --> , topLeft = Point2d.fromCoordinates ( 2, 3 )
    --> , topRight = Point2d.fromCoordinates ( 5, 3 )
    --> }

-}
fromExtrema : { minX : Quantity Float units, maxX : Quantity Float units, minY : Quantity Float units, maxY : Quantity Float units } -> Rectangle2d units coordinates
fromExtrema { minX, maxX, minY, maxY } =
    let
        computedAxes =
            Frame2d.atCoordinates
                (Quantity.midpoint minX maxX)
                (Quantity.midpoint minY maxY)

        computedDimensions =
            ( Quantity.abs (maxX |> Quantity.minus minX)
            , Quantity.abs (maxY |> Quantity.minus minY)
            )
    in
    Types.Rectangle2d
        { axes = computedAxes
        , dimensions = computedDimensions
        }


{-| Convert a rectangle to a [`Polygon2d`](Polygon2d#Polygon2d).
-}
toPolygon : Rectangle2d units coordinates -> Polygon2d units coordinates
toPolygon rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle
    in
    Polygon2d.singleLoop [ bottomLeft, bottomRight, topRight, topLeft ]


{-| Get the central axes of a rectangle as a `Frame2d`:

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.axes rectangle
    --> Frame2d.atCoordinates ( 3.5, 2 )

The origin point of the frame will be the center point of the rectangle.

-}
axes : Rectangle2d units coordinates -> Frame2d units coordinates RectangleCoordinates
axes (Types.Rectangle2d rectangle) =
    rectangle.axes


{-| Get the X axis of a rectangle;

    Rectangle2d.xAxis rectangle

is equivalent to

    Frame2d.xAxis (Rectangle2d.axes rectangle)

-}
xAxis : Rectangle2d units coordinates -> Axis2d units coordinates
xAxis rectangle =
    Frame2d.xAxis (axes rectangle)


{-| Get the Y axis of a rectangle;

    Rectangle2d.yAxis rectangle

is equivalent to

    Frame2d.yAxis (Rectangle2d.axes rectangle)

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
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.dimensions rectangle
    --> ( 3, 2 )

-}
dimensions : Rectangle2d units coordinates -> ( Quantity Float units, Quantity Float units )
dimensions (Types.Rectangle2d rectangle) =
    rectangle.dimensions


{-| Get the area of a rectangle:

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.area rectangle
    --> 6

-}
area : Rectangle2d units coordinates -> Quantity Float (Squared units)
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width |> Quantity.times height


{-| Get the vertices of a rectangle as a record. Note that 'bottom', 'top',
'left' and 'right' are with respect to the rectangle's axes, so the may not
correspond to global up/down or left/right if the rectangle has been rotated or
mirrored.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.vertices rectangle
    --> { bottomLeft = Point2d.fromCoordinates ( 2, 1 )
    --> , bottomRight = Point2d.fromCoordinates ( 5, 1 )
    --> , topLeft = Point2d.fromCoordinates ( 2, 3 )
    --> , topRight = Point2d.fromCoordinates ( 5, 3 )
    --> }

-}
vertices :
    Rectangle2d units coordinates
    ->
        { bottomLeft : Point2d units coordinates
        , bottomRight : Point2d units coordinates
        , topRight : Point2d units coordinates
        , topLeft : Point2d units coordinates
        }
vertices rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        halfWidth =
            Quantity.multiplyBy 0.5 width

        halfHeight =
            Quantity.multiplyBy 0.5 height
    in
    { bottomLeft =
        Point2d.fromCoordinatesIn localFrame
            (Quantity.negate halfWidth)
            (Quantity.negate halfHeight)
    , bottomRight =
        Point2d.fromCoordinatesIn localFrame
            halfWidth
            (Quantity.negate halfHeight)
    , topRight =
        Point2d.fromCoordinatesIn localFrame
            halfWidth
            halfHeight
    , topLeft =
        Point2d.fromCoordinatesIn localFrame
            (Quantity.negate halfWidth)
            halfHeight
    }


{-| Get the bottom left vertex of a rectangle;

    Rectangle2d.bottomLeftVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).bottomLeft

but is more efficient.

-}
bottomLeftVertex : Rectangle2d units coordinates -> Point2d units coordinates
bottomLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame
        (Quantity.multiplyBy -0.5 width)
        (Quantity.multiplyBy -0.5 height)


{-| Get the bottom right vertex of a rectangle;

    Rectangle2d.bottomRightVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).bottomRight

but is more efficient.

-}
bottomRightVertex : Rectangle2d units coordinates -> Point2d units coordinates
bottomRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame
        (Quantity.multiplyBy 0.5 width)
        (Quantity.multiplyBy -0.5 height)


{-| Get the top right vertex of a rectangle;

    Rectangle2d.topRightVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).topRight

but is more efficient.

-}
topRightVertex : Rectangle2d units coordinates -> Point2d units coordinates
topRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame
        (Quantity.multiplyBy 0.5 width)
        (Quantity.multiplyBy 0.5 height)


{-| Get the top left vertex of a rectangle;

    Rectangle2d.topLeftVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).topLeft

but is more efficient.

-}
topLeftVertex : Rectangle2d units coordinates -> Point2d units coordinates
topLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame
        (Quantity.multiplyBy -0.5 width)
        (Quantity.multiplyBy 0.5 height)


{-| Check if a rectangle contains a given point:

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    p1 =
        Point2d.fromCoordinates ( 3, 2 )

    p2 =
        Point2d.fromCoordinates ( 3, 4 )

    rectangle |> Rectangle2d.contains p1
    --> True

    rectangle |> Rectangle2d.contains p2
    --> False

-}
contains : Point2d units coordinates -> Rectangle2d units coordinates -> Bool
contains point rectangle =
    let
        ( width, height ) =
            dimensions rectangle

        halfWidth =
            Quantity.multiplyBy 0.5 width

        halfHeight =
            Quantity.multiplyBy 0.5 height

        rectangleFrame =
            axes rectangle

        ( x, y ) =
            Point2d.coordinatesIn rectangleFrame point
    in
    (Quantity.abs x |> Quantity.lessThanOrEqualTo halfWidth)
        && (Quantity.abs y |> Quantity.lessThanOrEqualTo halfHeight)


{-| Get the edges of a rectangle as a record. Note that 'bottom', 'top',
'left' and 'right' are with respect to the rectangle's axes, so the may not
correspond to global up/down or left/right if the rectangle has been rotated or
mirrored. The orientation of each edge is chosen so that it will be in a
counterclockwise direction (unless the rectangle has been mirrored):

  - The `bottom` edge is from the bottom left to bottom right vertex
  - The `right` edge is from the bottom right to top right vertex
  - The `top` edge is from the top right to top left vertex
  - The `left` edge is from the top left to bottom left vertex

(Note that this ordering will lead to each edge being in a clockwise direction
if the rectangle _has_ been mirrored.)

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.edges rectangle
    --> { bottom =
    -->     LineSegment2d.fromEndpoints
    -->         ( Point2d.fromCoordinates ( 2, 1 )
    -->         , Point2d.fromCoordinates ( 5, 1 )
    -->         )
    --> , right =
    -->     LineSegment2d.fromEndpoints
    -->         ( Point2d.fromCoordinates ( 5, 1 )
    -->         , Point2d.fromCoordinates ( 5, 3 )
    -->         )
    --> , top =
    -->     LineSegment2d.fromEndpoints
    -->         ( Point2d.fromCoordinates ( 5, 3 )
    -->         , Point2d.fromCoordinates ( 2, 3 )
    -->         )
    --> , left =
    -->     LineSegment2d.fromEndpoints
    -->         ( Point2d.fromCoordinates ( 2, 3 )
    -->         , Point2d.fromCoordinates ( 2, 1 )
    -->         )
    --> }

-}
edges :
    Rectangle2d units coordinates
    ->
        { bottom : LineSegment2d units coordinates
        , right : LineSegment2d units coordinates
        , top : LineSegment2d units coordinates
        , left : LineSegment2d units coordinates
        }
edges rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle
    in
    { bottom = LineSegment2d.from bottomLeft bottomRight
    , right = LineSegment2d.from bottomRight topRight
    , top = LineSegment2d.from topRight topLeft
    , left = LineSegment2d.from topLeft bottomLeft
    }


{-| Get the bottom edge of a rectangle;

    Rectangle2d.bottomEdge rectangle

is equivalent to

    (Rectangle2d.edges rectangle).bottom

but is more efficient.

-}
bottomEdge : Rectangle2d units coordinates -> LineSegment2d units coordinates
bottomEdge rectangle =
    LineSegment2d.from
        (bottomLeftVertex rectangle)
        (bottomRightVertex rectangle)


{-| Get the right edge of a rectangle;

    Rectangle2d.rightEdge rectangle

is equivalent to

    (Rectangle2d.edges rectangle).right

but is more efficient.

-}
rightEdge : Rectangle2d units coordinates -> LineSegment2d units coordinates
rightEdge rectangle =
    LineSegment2d.from
        (bottomRightVertex rectangle)
        (topRightVertex rectangle)


{-| Get the top edge of a rectangle;

    Rectangle2d.topEdge rectangle

is equivalent to

    (Rectangle2d.edges rectangle).top

but is more efficient.

-}
topEdge : Rectangle2d units coordinates -> LineSegment2d units coordinates
topEdge rectangle =
    LineSegment2d.from
        (topRightVertex rectangle)
        (topLeftVertex rectangle)


{-| Get the left edge of a rectangle;

    Rectangle2d.leftEdge rectangle

is equivalent to

    (Rectangle2d.edges rectangle).left

but is more efficient.

-}
leftEdge : Rectangle2d units coordinates -> LineSegment2d units coordinates
leftEdge rectangle =
    LineSegment2d.from
        (topLeftVertex rectangle)
        (bottomLeftVertex rectangle)


{-| Scale a rectangle about a given point by a given scale.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    rectangle
        |> Rectangle2d.scaleAbout Point2d.origin 2
    --> Rectangle2d.fromExtrema
    -->     { minX = 4
    -->     , maxX = 10
    -->     , minY = 2
    -->     , maxY = 6
    -->     }

Note that scaling by a negative value will flip the handedness of the
rectangle's axes, and therefore the order/direction of results from
`Rectangle2d.vertices` and `Rectangle2d.edges` will change.

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


{-| Rotate a rectangle around a given point by a given angle (in radians).

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 0
            , maxX = 1
            , minY = 0
            , maxY = 1
            }

    rotated =
        rectangle
            |> Rectangle2d.rotateAround Point2d.origin
                (degrees 45)

    Rectangle2d.centerPoint rotated
    --> Point2d.fromCoordinates ( 0, 0.7071 )

    Rectangle2d.xDirection rotated
    --> Direction2d.fromAngle (degrees 45)

    Rectangle2d.vertices rotated
    --> { bottomLeft =
    -->     Point2d.origin
    --> , bottomRight =
    -->     Point2d.fromCoordinates ( 0.7071, 0.7071 )
    --> , topRight =
    -->     Point2d.fromCoordinates ( 0, 1.4142 )
    --> , topLeft =
    -->     Point2d.fromCoordinates ( -0.7071, 0.7071 )
    --> }

-}
rotateAround : Point2d units coordinates -> Angle -> Rectangle2d units coordinates -> Rectangle2d units coordinates
rotateAround point angle rectangle =
    Types.Rectangle2d
        { axes = Frame2d.rotateAround point angle (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle by a given displacement.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    displacement =
        Vector2d.fromComponents ( 2, -3 )

    Rectangle2d.translateBy displacement rectangle
    --> Rectangle2d.fromExtrema
    -->     { minX = 4
    -->     , maxX = 7
    -->     , minY = -2
    -->     , maxY = 0
    -->     }

-}
translateBy : Vector2d units coordinates -> Rectangle2d units coordinates -> Rectangle2d units coordinates
translateBy displacement rectangle =
    Types.Rectangle2d
        { axes = Frame2d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle in a given direction by a given distance;

    Rectangle2d.translateIn direction distance

is equivalent to

    Rectangle2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> Rectangle2d units coordinates -> Rectangle2d units coordinates
translateIn direction distance rectangle =
    translateBy (Vector2d.withLength distance direction) rectangle


{-| Mirror a rectangle across a given axis.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    Rectangle2d.mirrorAcross Axis2d.x rectangle
    --> Rectangle2d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 5
    -->     , minY = -3
    -->     , maxY = -1
    -->     }

Note that this will flip the handedness of the rectangle's axes, and therefore
the order/direction of results from `Rectangle2d.vertices` and
`Rectangle2d.edges` will change.

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

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    Rectangle2d.placeIn localFrame rectangle
    --> Rectangle2d.fromExtrema
    -->     { minX = 3
    -->     , maxX = 6
    -->     , minY = 3
    -->     , maxY = 5
    -->     }

-}
placeIn : Frame2d units globalCoordinates localCoordinates -> Rectangle2d units localCoordinates -> Rectangle2d units globalCoordinates
placeIn frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Take a rectangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    rectangle =
        Rectangle2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 3
            }

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    Rectangle2d.relativeTo localFrame rectangle
    --> Rectangle2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 1
    -->     }

-}
relativeTo : Frame2d units globalCoordinates localCoordinates -> Rectangle2d units globalCoordinates -> Rectangle2d units localCoordinates
relativeTo frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Get the minimal bounding box containing a given rectangle. This have
exactly the same shape and size as the rectangle itself if the rectangle is
axis-aligned, but will be larger than the rectangle if the rectangle is at an
angle.

    square =
        Rectangle2d.fromExtrema
            { minX = 0
            , maxX = 1
            , minY = 0
            , maxY = 1
            }

    diamond =
        square
            |> Rectangle2d.rotateAround Point2d.origin
                (degrees 45)

    Rectangle2d.boundingBox diamond
    --> BoundingBox2d.fromExtrema
    -->     { minX = -0.7071
    -->     , maxX = 0.7071
    -->     , minY = 0
    -->     , maxY = 1.4142
    -->     }

-}
boundingBox : Rectangle2d units coordinates -> BoundingBox2d units coordinates
boundingBox rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle

        ( x1, y1 ) =
            Point2d.coordinates bottomLeft

        ( x2, y2 ) =
            Point2d.coordinates bottomRight

        ( x3, y3 ) =
            Point2d.coordinates topRight

        ( x4, y4 ) =
            Point2d.coordinates topLeft
    in
    BoundingBox2d.fromExtrema
        { minX = Quantity.min (Quantity.min x1 x2) (Quantity.min x3 x4)
        , maxX = Quantity.max (Quantity.max x1 x2) (Quantity.max x3 x4)
        , minY = Quantity.min (Quantity.min y1 y2) (Quantity.min y3 y4)
        , maxY = Quantity.max (Quantity.max y1 y2) (Quantity.max y3 y4)
        }
