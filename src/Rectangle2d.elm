--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Rectangle2d exposing
    ( Rectangle2d
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

@docs Rectangle2d


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

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Rectangle2d =
    Types.Rectangle2d


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
centeredOn : Frame2d -> ( Float, Float ) -> Rectangle2d
centeredOn givenAxes ( givenWidth, givenHeight ) =
    Types.Rectangle2d
        { axes = givenAxes
        , dimensions = ( abs givenWidth, abs givenHeight )
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
fromExtremaIn : Frame2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtremaIn localFrame { minX, maxX, minY, maxY } =
    let
        dx =
            maxX - minX

        dy =
            maxY - minY

        midX =
            minX + 0.5 * dx

        midY =
            minY + 0.5 * dy

        computedCenterPoint =
            Point2d.fromCoordinatesIn localFrame ( midX, midY )
    in
    Types.Rectangle2d
        { axes = Frame2d.moveTo computedCenterPoint localFrame
        , dimensions = ( abs dx, abs dy )
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
from : Point2d -> Point2d -> Rectangle2d
from firstPoint secondPoint =
    let
        computedCenterPoint =
            Point2d.midpoint firstPoint secondPoint

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    Types.Rectangle2d
        { axes = Frame2d.atPoint computedCenterPoint
        , dimensions = ( abs (x2 - x1), abs (y2 - y1) )
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
fromExtrema : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtrema { minX, maxX, minY, maxY } =
    let
        dx =
            maxX - minX

        dy =
            maxY - minY

        midX =
            minX + 0.5 * dx

        midY =
            minY + 0.5 * dy

        computedCenterPoint =
            Point2d.fromCoordinates ( midX, midY )
    in
    Types.Rectangle2d
        { axes = Frame2d.atPoint computedCenterPoint
        , dimensions = ( abs dx, abs dy )
        }


{-| Convert a rectangle to a [`Polygon2d`](Polygon2d#Polygon2d).
-}
toPolygon : Rectangle2d -> Polygon2d
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
axes : Rectangle2d -> Frame2d
axes (Types.Rectangle2d rectangle) =
    rectangle.axes


{-| Get the X axis of a rectangle;

    Rectangle2d.xAxis rectangle

is equivalent to

    Frame2d.xAxis (Rectangle2d.axes rectangle)

-}
xAxis : Rectangle2d -> Axis2d
xAxis rectangle =
    Frame2d.xAxis (axes rectangle)


{-| Get the Y axis of a rectangle;

    Rectangle2d.yAxis rectangle

is equivalent to

    Frame2d.yAxis (Rectangle2d.axes rectangle)

-}
yAxis : Rectangle2d -> Axis2d
yAxis rectangle =
    Frame2d.yAxis (axes rectangle)


{-| Get the center point of a rectangle.
-}
centerPoint : Rectangle2d -> Point2d
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
dimensions : Rectangle2d -> ( Float, Float )
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
area : Rectangle2d -> Float
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width * height


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
vertices : Rectangle2d -> { bottomLeft : Point2d, bottomRight : Point2d, topRight : Point2d, topLeft : Point2d }
vertices rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2
    in
    { bottomLeft =
        Point2d.fromCoordinatesIn localFrame ( -halfWidth, -halfHeight )
    , bottomRight =
        Point2d.fromCoordinatesIn localFrame ( halfWidth, -halfHeight )
    , topRight =
        Point2d.fromCoordinatesIn localFrame ( halfWidth, halfHeight )
    , topLeft =
        Point2d.fromCoordinatesIn localFrame ( -halfWidth, halfHeight )
    }


{-| Get the bottom left vertex of a rectangle;

    Rectangle2d.bottomLeftVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).bottomLeft

but is more efficient.

-}
bottomLeftVertex : Rectangle2d -> Point2d
bottomLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( -width / 2, -height / 2 )


{-| Get the bottom right vertex of a rectangle;

    Rectangle2d.bottomRightVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).bottomRight

but is more efficient.

-}
bottomRightVertex : Rectangle2d -> Point2d
bottomRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( width / 2, -height / 2 )


{-| Get the top right vertex of a rectangle;

    Rectangle2d.topRightVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).topRight

but is more efficient.

-}
topRightVertex : Rectangle2d -> Point2d
topRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( width / 2, height / 2 )


{-| Get the top left vertex of a rectangle;

    Rectangle2d.topLeftVertex rectangle

is equivalent to

    (Rectangle2d.vertices rectangle).topLeft

but is more efficient.

-}
topLeftVertex : Rectangle2d -> Point2d
topLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( -width / 2, height / 2 )


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
contains : Point2d -> Rectangle2d -> Bool
contains point rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        ( x, y ) =
            Point2d.coordinates (Point2d.relativeTo localFrame point)
    in
    abs x <= width / 2 && abs y <= height / 2


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
edges : Rectangle2d -> { bottom : LineSegment2d, right : LineSegment2d, top : LineSegment2d, left : LineSegment2d }
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
bottomEdge : Rectangle2d -> LineSegment2d
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
rightEdge : Rectangle2d -> LineSegment2d
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
topEdge : Rectangle2d -> LineSegment2d
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
leftEdge : Rectangle2d -> LineSegment2d
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
scaleAbout : Point2d -> Float -> Rectangle2d -> Rectangle2d
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
            abs (scale * currentWidth)

        newHeight =
            abs (scale * currentHeight)
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
rotateAround : Point2d -> Float -> Rectangle2d -> Rectangle2d
rotateAround point angle =
    let
        rotateFrame =
            Frame2d.rotateAround point angle
    in
    \rectangle ->
        Types.Rectangle2d
            { axes = rotateFrame (axes rectangle)
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
translateBy : Vector2d -> Rectangle2d -> Rectangle2d
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
translateIn : Direction2d -> Float -> Rectangle2d -> Rectangle2d
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
mirrorAcross : Axis2d -> Rectangle2d -> Rectangle2d
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
placeIn : Frame2d -> Rectangle2d -> Rectangle2d
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
relativeTo : Frame2d -> Rectangle2d -> Rectangle2d
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
boundingBox : Rectangle2d -> BoundingBox2d
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
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        }
