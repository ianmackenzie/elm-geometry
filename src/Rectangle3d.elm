module Rectangle3d exposing
    ( Rectangle3d
    , on, centeredOn
    , dimensions, axes, xAxis, yAxis, centerPoint, area, vertices, edges, boundingBox
    , interpolate
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , randomPoint
    )

{-| The `Rectangle3d` type is an extension of `Rectangle2d` that exists in 3D.
In most cases it will be most convenient to create a `Rectangle3d` by creating
a `Rectangle2d` on a particular `SketchPlane3d`, using [`Rectangle3d.on`](#on).
This module then contains functionality for:

  - Finding the edges and vertices of rectangles
  - Interpolating points within rectangles
  - Scaling, rotating, translating, mirroring and projecting rectangles
  - Converting rectangles between different coordinate systems

@docs Rectangle3d


# Construction

@docs on, centeredOn


# Properties

@docs dimensions, axes, xAxis, yAxis, centerPoint, area, vertices, edges, boundingBox


# Interpolation

@docs interpolate


# Transformation

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Random point generation

@docs randomPoint

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Rate, Squared)
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Rectangle3d units coordinates =
    Types.Rectangle3d units coordinates


{-| Construct a 3D rectangle lying _on_ a sketch plane by providing a 2D
rectangle specified in XY coordinates _within_ the sketch plane. For example,
to create a 3D rectangle lying on the YZ plane:

    rectangle =
        Rectangle3d.on SketchPlane3d.yz <|
            Rectangle2d.from
                (Point2d.meters 1 2)
                (Point2d.meters 3 4)

    Rectangle3d.vertices rectangle
    --> [ Point3d.meters 0 1 2
    --> , Point3d.meters 0 3 2
    --> , Point3d.meters 0 3 4
    --> , Point3d.meters 0 1 4
    --> ]

-}
on :
    SketchPlane3d units coordinates3d { defines : coordinates2d }
    -> Rectangle2d units coordinates2d
    -> Rectangle3d units coordinates3d
on sketchPlane rectangle2d =
    Types.Rectangle3d
        { axes = SketchPlane3d.on sketchPlane (Rectangle2d.axes rectangle2d)
        , dimensions = Rectangle2d.dimensions rectangle2d
        }


{-| Construct a rectangle centered on the axes of the given sketch plane, with
the given overall dimensions.
-}
centeredOn :
    SketchPlane3d units coordinates defines
    -> ( Quantity Float units, Quantity Float units )
    -> Rectangle3d units coordinates
centeredOn givenAxes ( givenWidth, givenHeight ) =
    Types.Rectangle3d
        { axes = SketchPlane3d.copy givenAxes
        , dimensions = ( Quantity.abs givenWidth, Quantity.abs givenHeight )
        }


{-| Convert a rectangle from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at :
    Quantity Float (Rate units2 units1)
    -> Rectangle3d units1 coordinates
    -> Rectangle3d units2 coordinates
at rate (Types.Rectangle3d rectangle) =
    let
        ( width, height ) =
            rectangle.dimensions
    in
    Types.Rectangle3d
        { axes = SketchPlane3d.at rate rectangle.axes
        , dimensions =
            ( Quantity.abs (Quantity.at rate width)
            , Quantity.abs (Quantity.at rate height)
            )
        }


{-| Convert a rectangle from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ :
    Quantity Float (Rate units1 units2)
    -> Rectangle3d units1 coordinates
    -> Rectangle3d units2 coordinates
at_ rate rectangle =
    at (Quantity.inverse rate) rectangle


{-| Get the central axes of a rectangle as a `SketchPlane3d`. The origin point
of the sketch plane will be the center point of the rectangle.
-}
axes : Rectangle3d units coordinates -> SketchPlane3d units coordinates defines
axes (Types.Rectangle3d rectangle) =
    SketchPlane3d.copy rectangle.axes


{-| Get the X axis of a rectangle.
-}
xAxis : Rectangle3d units coordinates -> Axis3d units coordinates
xAxis rectangle =
    SketchPlane3d.xAxis (axes rectangle)


{-| Get the Y axis of a rectangle.
-}
yAxis : Rectangle3d units coordinates -> Axis3d units coordinates
yAxis rectangle =
    SketchPlane3d.yAxis (axes rectangle)


{-| Get the center point of a rectangle.
-}
centerPoint : Rectangle3d units coordinates -> Point3d units coordinates
centerPoint rectangle =
    SketchPlane3d.originPoint (axes rectangle)


{-| Get the overall dimensions (width and height) of a rectangle.
-}
dimensions :
    Rectangle3d units coordinates
    -> ( Quantity Float units, Quantity Float units )
dimensions (Types.Rectangle3d rectangle) =
    rectangle.dimensions


{-| Get the area of a rectangle.
-}
area : Rectangle3d units coordinates -> Quantity Float (Squared units)
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width |> Quantity.times height


{-| Get the vertices of a rectangle as a list. The vertices will be returned
in counterclockwise order with respect to the `SketchPlane3d` defining the
rectangle's axes.
-}
vertices : Rectangle3d units coordinates -> List (Point3d units coordinates)
vertices rectangle =
    let
        sketchPlane =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        x =
            Quantity.half width

        y =
            Quantity.half height
    in
    [ Point3d.xyOn sketchPlane (Quantity.negate x) (Quantity.negate y)
    , Point3d.xyOn sketchPlane x (Quantity.negate y)
    , Point3d.xyOn sketchPlane x y
    , Point3d.xyOn sketchPlane (Quantity.negate x) y
    ]


{-| Get the edges of a rectangle as a list. The edges will be returned
in counterclockwise order with respect to the `SketchPlane3d` defining the
rectangle's axes.
-}
edges : Rectangle3d units coordinates -> List (LineSegment3d units coordinates)
edges rectangle =
    let
        sketchPlane =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        x =
            Quantity.half width

        y =
            Quantity.half height

        p1 =
            Point3d.xyOn sketchPlane (Quantity.negate x) (Quantity.negate y)

        p2 =
            Point3d.xyOn sketchPlane x (Quantity.negate y)

        p3 =
            Point3d.xyOn sketchPlane x y

        p4 =
            Point3d.xyOn sketchPlane (Quantity.negate x) y
    in
    [ LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p4 p1
    ]


{-| Scale a rectangle about a given point by a given scale. Note that scaling by
a negative value will flip the directions of the rectangle's axes.
-}
scaleAbout : Point3d units coordinates -> Float -> Rectangle3d units coordinates -> Rectangle3d units coordinates
scaleAbout point scale rectangle =
    let
        currentAxes =
            axes rectangle

        currentXDirection =
            SketchPlane3d.xDirection currentAxes

        currentYDirection =
            SketchPlane3d.yDirection currentAxes

        newCenterPoint =
            Point3d.scaleAbout point scale (SketchPlane3d.originPoint currentAxes)

        newAxes =
            if scale >= 0 then
                SketchPlane3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    }

            else
                SketchPlane3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction3d.reverse currentXDirection
                    , yDirection = Direction3d.reverse currentYDirection
                    }

        ( currentWidth, currentHeight ) =
            dimensions rectangle

        newWidth =
            Quantity.abs (Quantity.multiplyBy scale currentWidth)

        newHeight =
            Quantity.abs (Quantity.multiplyBy scale currentHeight)
    in
    Types.Rectangle3d
        { axes = newAxes
        , dimensions = ( newWidth, newHeight )
        }


{-| Rotate a rectangle around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Rectangle3d units coordinates -> Rectangle3d units coordinates
rotateAround axis angle rectangle =
    Types.Rectangle3d
        { axes = SketchPlane3d.rotateAround axis angle (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle by a given displacement.
-}
translateBy :
    Vector3d units coordinates
    -> Rectangle3d units coordinates
    -> Rectangle3d units coordinates
translateBy displacement rectangle =
    Types.Rectangle3d
        { axes = SketchPlane3d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Translate a rectangle in a given direction by a given distance.
-}
translateIn :
    Direction3d coordinates
    -> Quantity Float units
    -> Rectangle3d units coordinates
    -> Rectangle3d units coordinates
translateIn direction distance rectangle =
    translateBy (Vector3d.withLength distance direction) rectangle


{-| Mirror a rectangle across a given plane.
-}
mirrorAcross : Plane3d units coordinates -> Rectangle3d units coordinates -> Rectangle3d units coordinates
mirrorAcross axis rectangle =
    Types.Rectangle3d
        { axes = SketchPlane3d.mirrorAcross axis (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Take a rectangle considered to be defined in local coordinates relative to a
given reference frame, and return that rectangle expressed in global
coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Rectangle3d units localCoordinates
    -> Rectangle3d units globalCoordinates
placeIn frame rectangle =
    Types.Rectangle3d
        { axes = SketchPlane3d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Take a rectangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Rectangle3d units globalCoordinates
    -> Rectangle3d units localCoordinates
relativeTo frame rectangle =
    Types.Rectangle3d
        { axes = SketchPlane3d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| Get the minimal bounding box containing a given rectangle.
-}
boundingBox : Rectangle3d units coordinates -> BoundingBox3d units coordinates
boundingBox rectangle =
    let
        sketchPlane =
            axes rectangle

        (Types.Point3d p0) =
            SketchPlane3d.originPoint sketchPlane

        (Types.Direction3d i) =
            SketchPlane3d.xDirection sketchPlane

        ix =
            abs i.x

        iy =
            abs i.y

        iz =
            abs i.z

        (Types.Direction3d j) =
            SketchPlane3d.yDirection sketchPlane

        jx =
            abs j.x

        jy =
            abs j.y

        jz =
            abs j.z

        ( Quantity width, Quantity height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2

        minX =
            p0.x - ix * halfWidth - jx * halfHeight

        minY =
            p0.y - iy * halfWidth - jy * halfHeight

        minZ =
            p0.z - iz * halfWidth - jz * halfHeight

        maxX =
            p0.x + ix * halfWidth + jx * halfHeight

        maxY =
            p0.y + iy * halfWidth + jy * halfHeight

        maxZ =
            p0.z + iz * halfWidth + jz * halfHeight
    in
    Types.BoundingBox3d
        { minX = Quantity minX
        , maxX = Quantity maxX
        , minY = Quantity minY
        , maxY = Quantity maxY
        , minZ = Quantity minZ
        , maxZ = Quantity maxZ
        }


{-| Interpolate within a rectangle based on coordinates which range from 0 to 1.
For example, the four vertices of a given rectangle are

    [ Rectangle3d.interpolate rectangle 0 0
    , Rectangle3d.interpolate rectangle 1 0
    , Rectangle3d.interpolate rectangle 1 1
    , Rectangle3d.interpolate rectangle 0 1
    ]

and its center point is

    Rectangle3d.interpolate rectangle 0.5 0.5

-}
interpolate :
    Rectangle3d units coordinates
    -> Float
    -> Float
    -> Point3d units coordinates
interpolate rectangle u v =
    let
        ( width, height ) =
            dimensions rectangle
    in
    Point3d.xyOn (axes rectangle)
        (Quantity.multiplyBy (u - 0.5) width)
        (Quantity.multiplyBy (v - 0.5) height)


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for points within a given rectangle.
-}
randomPoint : Rectangle3d units coordinates -> Generator (Point3d units coordinates)
randomPoint rectangle =
    Random.map2 (interpolate rectangle) (Random.float 0 1) (Random.float 0 1)
