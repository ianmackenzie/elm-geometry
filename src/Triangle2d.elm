--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Triangle2d exposing
    ( Triangle2d
    , fromVertices, from
    , vertices, edges, centroid, area, counterclockwiseArea, clockwiseArea, boundingBox, circumcircle
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, mapVertices
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Triangle2d` represents a triangle in 2D space, and is defined by its
three vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating and mirroring triangles
  - Converting triangles between different coordinate systems

@docs Triangle2d


# Constructors

@docs fromVertices, from


# Properties

@docs vertices, edges, centroid, area, counterclockwiseArea, clockwiseArea, boundingBox, circumcircle


# Queries

@docs contains


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, mapVertices


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias Triangle2d units coordinates =
    Types.Triangle2d units coordinates


{-| Construct a triangle from its three vertices:

    exampleTriangle =
        Triangle2d.fromVertices
            ( Point2d.meters 1 1
            , Point2d.meters 2 1
            , Point2d.meters 1 3
            )

-}
fromVertices : ( Point2d units coordinates, Point2d units coordinates, Point2d units coordinates ) -> Triangle2d units coordinates
fromVertices givenVertices =
    Types.Triangle2d givenVertices


{-| Construct a triangle from the first point, to the second, to the third:

    exampleTriangle =
        Triangle2d.from
            (Point2d.meters 1 1)
            (Point2d.meters 2 1)
            (Point2d.meters 1 3)

Useful with `map3` functions such as `Json.Decode.map3`.

-}
from : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Triangle2d units coordinates
from p1 p2 p3 =
    Types.Triangle2d ( p1, p2, p3 )


{-| Convert a triangle from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Triangle2d units1 coordinates -> Triangle2d units2 coordinates
at rate (Types.Triangle2d ( p1, p2, p3 )) =
    Types.Triangle2d
        ( Point2d.at rate p1
        , Point2d.at rate p2
        , Point2d.at rate p3
        )


{-| Convert a triangle from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Triangle2d units1 coordinates -> Triangle2d units2 coordinates
at_ rate triangle =
    at (Quantity.inverse rate) triangle


{-| Get the vertices of a triangle.

    ( p1, p2, p3 ) =
        Triangle2d.vertices exampleTriangle

-}
vertices : Triangle2d units coordinates -> ( Point2d units coordinates, Point2d units coordinates, Point2d units coordinates )
vertices (Types.Triangle2d triangleVertices) =
    triangleVertices


{-| Get the edges of a triangle: from the first vertex to the second, from the
second to the third, and from the third back to the first.


    ( e1, e2, e3 ) =
        Triangle2d.edges exampleTriangle

    --> e1 =
    -->     LineSegment2d.from
    -->         (Point2d.meters 1 1)
    -->         (Point2d.meters 2 1)
    -->
    --> e2 =
    -->     LineSegment2d.from
    -->         (Point2d.meters 2 1)
    -->         (Point2d.meters 1 3)
    -->
    --> e3 =
    -->     LineSegment2d.from
    -->         (Point2d.meters 1 3)
    -->         (Point2d.meters 1 1)

-}
edges : Triangle2d units coordinates -> ( LineSegment2d units coordinates, LineSegment2d units coordinates, LineSegment2d units coordinates )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    ( LineSegment2d.fromEndpoints ( p1, p2 )
    , LineSegment2d.fromEndpoints ( p2, p3 )
    , LineSegment2d.fromEndpoints ( p3, p1 )
    )


{-| Get the centroid (center of mass) of a triangle.

    Triangle2d.centroid exampleTriangle
    --> Point2d.meters 1.3333 1.6667

-}
centroid : Triangle2d units coordinates -> Point2d units coordinates
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Point2d.centroid3 p1 p2 p3


{-| Check whether a given point is inside a given triangle. It does not matter
whether the triangle's vertices are in clockwise or counterclockwise order.
-}
contains : Point2d units coordinates -> Triangle2d units coordinates -> Bool
contains point triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        crossProduct startVertex endVertex =
            let
                vectorToPoint =
                    Vector2d.from startVertex point

                segmentVector =
                    Vector2d.from startVertex endVertex
            in
            segmentVector |> Vector2d.cross vectorToPoint

        firstProduct =
            crossProduct p1 p2

        secondProduct =
            crossProduct p2 p3

        thirdProduct =
            crossProduct p3 p1
    in
    ((firstProduct |> Quantity.greaterThanOrEqualTo Quantity.zero)
        && (secondProduct |> Quantity.greaterThanOrEqualTo Quantity.zero)
        && (thirdProduct |> Quantity.greaterThanOrEqualTo Quantity.zero)
    )
        || ((firstProduct |> Quantity.lessThanOrEqualTo Quantity.zero)
                && (secondProduct |> Quantity.lessThanOrEqualTo Quantity.zero)
                && (thirdProduct |> Quantity.lessThanOrEqualTo Quantity.zero)
           )


{-| Get the area of a triangle. The result will always be positive regardless of
whether the triangle's vertices are in clockwise or counterclockwise order.
-}
area : Triangle2d units coordinates -> Quantity Float (Squared units)
area =
    counterclockwiseArea >> Quantity.abs


{-| Get the signed area of a triangle, returning a positive value if the
triangle's vertices are in counterclockwise order and a negative value
otherwise.

    Triangle2d.counterclockwiseArea exampleTriangle
    --> Area.squareMeters 1.0

-}
counterclockwiseArea : Triangle2d units coordinates -> Quantity Float (Squared units)
counterclockwiseArea triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector2d.from p1 p2

        secondVector =
            Vector2d.from p1 p3
    in
    Quantity.multiplyBy 0.5 (firstVector |> Vector2d.cross secondVector)


{-| Get the signed area of a triangle, returning a positive value if the
triangle's vertices are in clockwise order and a negative value otherwise.

    Triangle2d.clockwiseArea exampleTriangle
    --> Area.squareMeters -1.0

-}
clockwiseArea : Triangle2d units coordinates -> Quantity Float (Squared units)
clockwiseArea triangle =
    Quantity.negate (counterclockwiseArea triangle)


{-| Scale a triangle about a given point by a given scale. Note that scaling by
a negative value will result in the 'winding direction' of the triangle being
flipped - if the triangle's vertices were in counterclockwise order before the
negative scaling, they will be in clockwise order afterwards and vice versa.
-}
scaleAbout : Point2d units coordinates -> Float -> Triangle2d units coordinates -> Triangle2d units coordinates
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale)


{-| Rotate a triangle around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Triangle2d units coordinates -> Triangle2d units coordinates
rotateAround centerPoint angle =
    mapVertices (Point2d.rotateAround centerPoint angle)


{-| Translate a triangle by a given displacement.
-}
translateBy : Vector2d units coordinates -> Triangle2d units coordinates -> Triangle2d units coordinates
translateBy vector =
    mapVertices (Point2d.translateBy vector)


{-| Translate a triangle in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Triangle2d units coordinates -> Triangle2d units coordinates
translateIn direction distance triangle =
    translateBy (Vector2d.withLength distance direction) triangle


{-| Mirror a triangle across a given axis. Note that mirroring a triangle will
result in its 'winding direction' being flipped - if the triangle's vertices
were in counterclockwise order before mirroring, they will be in clockwise order
afterwards and vice versa.
-}
mirrorAcross : Axis2d units coordinates -> Triangle2d units coordinates -> Triangle2d units coordinates
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis)


{-| Transform each vertex of a triangle by a given function and create a new
triangle from the resulting points. Most other transformation functions can be
defined in terms of `mapVertices`; for example,

    Triangle2d.mirrorAcross axis

is equivalent to

    Triangle2d.mapVertices (Point2d.mirrorAcross axis)

-}
mapVertices : (Point2d unitsA coordinatesA -> Point2d unitsB coordinatesB) -> Triangle2d unitsA coordinatesA -> Triangle2d unitsB coordinatesB
mapVertices function (Types.Triangle2d ( p1, p2, p3 )) =
    Types.Triangle2d ( function p1, function p2, function p3 )


{-| Take a triangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Triangle2d units globalCoordinates -> Triangle2d units localCoordinates
relativeTo frame =
    mapVertices (Point2d.relativeTo frame)


{-| Take a triangle considered to be defined in local coordinates relative to a
given reference frame, and return that triangle expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Triangle2d units localCoordinates -> Triangle2d units globalCoordinates
placeIn frame triangle =
    mapVertices (Point2d.placeIn frame) triangle


{-| Get the minimal bounding box containing a given triangle.

    Triangle2d.boundingBox exampleTriangle
    --> BoundingBox2d.from
    -->     (Point2d.meters 1 1)
    -->     (Point2d.meters 2 3)

-}
boundingBox : Triangle2d units coordinates -> BoundingBox2d units coordinates
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        x1 =
            Point2d.xCoordinate p1

        y1 =
            Point2d.yCoordinate p1

        x2 =
            Point2d.xCoordinate p2

        y2 =
            Point2d.yCoordinate p2

        x3 =
            Point2d.xCoordinate p3

        y3 =
            Point2d.yCoordinate p3
    in
    BoundingBox2d.fromExtrema
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        }


{-| Attempt to find the circumcircle of a triangle, a circle that passes through
each of the triangle's vertices;

    Triangle2d.circumcircle triangle

is equivalent to

    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Circle2d.throughPoints p1 p2 p3

If the triangle is degenerate (its three vertices are collinear), returns
`Nothing`.

-}
circumcircle : Triangle2d units coordinates -> Maybe (Circle2d units coordinates)
circumcircle triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Circle2d.throughPoints p1 p2 p3
