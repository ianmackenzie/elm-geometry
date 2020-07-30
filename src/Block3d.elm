--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Block3d exposing
    ( Block3d
    , with, from, centeredOn, fromBoundingBox
    , dimensions, axes, xAxis, yAxis, zAxis, centerPoint, volume, vertices, edges, boundingBox
    , interpolate
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Block3d` represents a rectangular block in 3D space. This module contains
block-related functionality such as:

  - Constructing blocks in various ways
  - Extracting block vertices and edges
  - Scaling, rotating, translating and mirroring blocks
  - Converting blocks between different coordinate systems

Unlike bounding boxes, blocks are _not_ constrained to be axis-aligned -
they can have arbitrary orientation and so can be rotated, mirrored etc.

@docs Block3d


# Construction

@docs with, from, centeredOn, fromBoundingBox


# Properties

@docs dimensions, axes, xAxis, yAxis, zAxis, centerPoint, volume, vertices, edges, boundingBox


# Interpolation

@docs interpolate


# Querying

@docs contains


# Transformation

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

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
import Quantity exposing (Cubed, Quantity(..), Rate)
import Vector3d exposing (Vector3d)


{-| -}
type alias Block3d units coordinates =
    Types.Block3d units coordinates


{-| Construct an axis-aligned block given the X, Y and Z coordinates of two
diagonally opposite vertices. The X, Y and Z directions of the resulting block
are determined by the order of the given values. For example, if `x1 <= x2`,
then the block's X direction will be `Direction3d.positiveX`, otherwise it will
be `Direction3d.negativeX`, and similar for Y and Z. Therefore, something like

    Block3d.with
        { x1 = Length.meters 0
        , x2 = Length.meters 500
        , y1 = Length.meters 300
        , y2 = Length.meters 0
        , z1 = Length.meters 100
        , z2 = Length.meters 200
        }

would have its X direction equal to `Direction3d.positiveX`, its Y direction
equal to `Direction3d.negativeY`, and its Z direction equal to `Direction3d.positiveZ`.

-}
with :
    { x1 : Quantity Float units
    , y1 : Quantity Float units
    , z1 : Quantity Float units
    , x2 : Quantity Float units
    , y2 : Quantity Float units
    , z2 : Quantity Float units
    }
    -> Block3d units coordinates
with { x1, y1, z1, x2, y2, z2 } =
    axisAligned x1 y1 z1 x2 y2 z2


{-| Construct an axis-aligned block stretching from one point to another;

    Block3d.from p1 p2

is equivalent to

    Block3d.with
        { x1 = Point3d.xCoordinate p1
        , y1 = Point3d.yCoordinate p1
        , z1 = Point3d.zCoordinate p1
        , x2 = Point3d.xCoordinate p2
        , y2 = Point3d.yCoordinate p2
        , z2 = Point3d.zCoordinate p2
        }

and so the same logic about the resulting block's X, Y and Z directions
applies (see above for details).

-}
from : Point3d units coordinates -> Point3d units coordinates -> Block3d units coordinates
from p1 p2 =
    axisAligned
        (Point3d.xCoordinate p1)
        (Point3d.yCoordinate p1)
        (Point3d.zCoordinate p1)
        (Point3d.xCoordinate p2)
        (Point3d.yCoordinate p2)
        (Point3d.zCoordinate p2)


{-| Construct a frame centered on the given axes, with the given overall
dimensions (length/width/height):

    block =
        Block3d.centeredOn Frame3d.atOrigin
            ( meters 20, meters 10, meters 30 )

    Block3d.vertices block
    --> [ Point3d.meters -10 -5 -15
    --> , Point3d.meters -10 -5 15
    --> , Point3d.meters -10 5 -15
    --> , Point3d.meters -10 5 15
    --> , Point3d.meters 10 -5 -15
    --> , Point3d.meters 10 -5 15
    --> , Point3d.meters 10 5 -15
    --> , Point3d.meters 10 5 15
    --> ]

-}
centeredOn :
    Frame3d units coordinates defines
    -> ( Quantity Float units, Quantity Float units, Quantity Float units )
    -> Block3d units coordinates
centeredOn givenAxes ( xDimension, yDimension, zDimension ) =
    Types.Block3d
        { axes = Frame3d.copy givenAxes
        , dimensions = ( Quantity.abs xDimension, Quantity.abs yDimension, Quantity.abs zDimension )
        }


{-| Convert a `BoundingBox3d` to the equivalent axis-aligned `Block3d`.
-}
fromBoundingBox : BoundingBox3d units coordinates -> Block3d units coordinates
fromBoundingBox givenBox =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema givenBox
    in
    axisAligned minX minY minZ maxX maxY maxZ


axisAligned :
    Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
    -> Block3d units coordinates
axisAligned x1 y1 z1 x2 y2 z2 =
    let
        computedCenterPoint =
            Point3d.xyz
                (Quantity.midpoint x1 x2)
                (Quantity.midpoint y1 y2)
                (Quantity.midpoint z1 z2)

        computedXDirection =
            if x2 |> Quantity.greaterThanOrEqualTo x1 then
                Direction3d.positiveX

            else
                Direction3d.negativeX

        computedYDirection =
            if y2 |> Quantity.greaterThanOrEqualTo y1 then
                Direction3d.positiveY

            else
                Direction3d.negativeY

        computedZDirection =
            if z2 |> Quantity.greaterThanOrEqualTo z1 then
                Direction3d.positiveZ

            else
                Direction3d.negativeZ

        computedAxes =
            Frame3d.unsafe
                { originPoint = computedCenterPoint
                , xDirection = computedXDirection
                , yDirection = computedYDirection
                , zDirection = computedZDirection
                }

        computedDimensions =
            ( Quantity.abs (x2 |> Quantity.minus x1)
            , Quantity.abs (y2 |> Quantity.minus y1)
            , Quantity.abs (z2 |> Quantity.minus z1)
            )
    in
    Types.Block3d
        { axes = computedAxes
        , dimensions = computedDimensions
        }


{-| Convert a block from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Block3d units1 coordinates -> Block3d units2 coordinates
at rate (Types.Block3d block) =
    let
        ( xDimension, yDimension, zDimension ) =
            block.dimensions
    in
    Types.Block3d
        { axes = Frame3d.at rate block.axes
        , dimensions =
            ( Quantity.abs (Quantity.at rate xDimension)
            , Quantity.abs (Quantity.at rate yDimension)
            , Quantity.abs (Quantity.at rate zDimension)
            )
        }


{-| Convert a block from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Block3d units1 coordinates -> Block3d units2 coordinates
at_ rate block =
    at (Quantity.inverse rate) block


{-| Get the central axes of a block as a `Frame3d`. The origin point of the
frame will be the center point of the block.
-}
axes : Block3d units coordinates -> Frame3d units coordinates defines
axes (Types.Block3d block) =
    Frame3d.copy block.axes


{-| Get the X axis of a block.
-}
xAxis : Block3d units coordinates -> Axis3d units coordinates
xAxis block =
    Frame3d.xAxis (axes block)


{-| Get the Y axis of a block.
-}
yAxis : Block3d units coordinates -> Axis3d units coordinates
yAxis block =
    Frame3d.yAxis (axes block)


{-| Get the Z axis of a block.
-}
zAxis : Block3d units coordinates -> Axis3d units coordinates
zAxis block =
    Frame3d.zAxis (axes block)


{-| Get the center point of a block.
-}
centerPoint : Block3d units coordinates -> Point3d units coordinates
centerPoint block =
    Frame3d.originPoint (axes block)


{-| Get the overall dimensions (e.g. length, width, height) of a block.
-}
dimensions : Block3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
dimensions (Types.Block3d block) =
    block.dimensions


{-| Get the volume of a block.
-}
volume : Block3d units coordinates -> Quantity Float (Cubed units)
volume block =
    let
        ( xDimension, yDimension, zDimension ) =
            dimensions block
    in
    xDimension |> Quantity.times yDimension |> Quantity.times zDimension


{-| Get the 8 vertices of a block as a list. The order of the vertices
returned is unspecified.
-}
vertices : Block3d units coordinates -> List (Point3d units coordinates)
vertices block =
    let
        localFrame =
            axes block

        ( xDimension, yDimension, zDimension ) =
            dimensions block

        positiveX =
            Quantity.half xDimension

        negativeX =
            Quantity.negate positiveX

        positiveY =
            Quantity.half yDimension

        negativeY =
            Quantity.negate positiveY

        positiveZ =
            Quantity.half zDimension

        negativeZ =
            Quantity.negate positiveZ
    in
    [ Point3d.xyzIn localFrame negativeX negativeY negativeZ
    , Point3d.xyzIn localFrame negativeX negativeY positiveZ
    , Point3d.xyzIn localFrame negativeX positiveY negativeZ
    , Point3d.xyzIn localFrame negativeX positiveY positiveZ
    , Point3d.xyzIn localFrame positiveX negativeY negativeZ
    , Point3d.xyzIn localFrame positiveX negativeY positiveZ
    , Point3d.xyzIn localFrame positiveX positiveY negativeZ
    , Point3d.xyzIn localFrame positiveX positiveY positiveZ
    ]


{-| Check if a block contains a given point:

    block =
        Block3d.from
            (Point3d.meters 2 1 0)
            (Point3d.meters 5 3 4)

    block |> Block3d.contains (Point3d.meters 3 2 3)
    --> True

    block |> Block3d.contains (Point3d.meters 3 4 1)
    --> False

-}
contains : Point3d units coordinates -> Block3d units coordinates -> Bool
contains point block =
    let
        ( xDimension, yDimension, zDimension ) =
            dimensions block

        localFrame =
            axes block

        x =
            Point3d.xCoordinateIn localFrame point

        y =
            Point3d.yCoordinateIn localFrame point

        z =
            Point3d.zCoordinateIn localFrame point
    in
    (Quantity.abs x |> Quantity.lessThanOrEqualTo (Quantity.half xDimension))
        && (Quantity.abs y |> Quantity.lessThanOrEqualTo (Quantity.half yDimension))
        && (Quantity.abs z |> Quantity.lessThanOrEqualTo (Quantity.half zDimension))


{-| Get the 12 edges of a block as a list. The orientation of each edge and the
order in which they are returned are unspecified.
-}
edges : Block3d units coordinates -> List (LineSegment3d units coordinates)
edges block =
    let
        localFrame =
            axes block

        ( xDimension, yDimension, zDimension ) =
            dimensions block

        positiveX =
            Quantity.half xDimension

        negativeX =
            Quantity.negate positiveX

        positiveY =
            Quantity.half yDimension

        negativeY =
            Quantity.negate positiveY

        positiveZ =
            Quantity.half zDimension

        negativeZ =
            Quantity.negate positiveZ

        p1 =
            Point3d.xyzIn localFrame negativeX negativeY negativeZ

        p2 =
            Point3d.xyzIn localFrame negativeX negativeY positiveZ

        p3 =
            Point3d.xyzIn localFrame negativeX positiveY negativeZ

        p4 =
            Point3d.xyzIn localFrame negativeX positiveY positiveZ

        p5 =
            Point3d.xyzIn localFrame positiveX negativeY negativeZ

        p6 =
            Point3d.xyzIn localFrame positiveX negativeY positiveZ

        p7 =
            Point3d.xyzIn localFrame positiveX positiveY negativeZ

        p8 =
            Point3d.xyzIn localFrame positiveX positiveY positiveZ
    in
    [ LineSegment3d.from p1 p3
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p4 p2
    , LineSegment3d.from p2 p1
    , LineSegment3d.from p1 p5
    , LineSegment3d.from p3 p7
    , LineSegment3d.from p4 p8
    , LineSegment3d.from p2 p6
    , LineSegment3d.from p5 p7
    , LineSegment3d.from p7 p8
    , LineSegment3d.from p8 p6
    , LineSegment3d.from p6 p5
    ]


{-| Scale a block about a given point by a given scale. Note that scaling by
a negative value will flip the handedness of the block's axes, and therefore
the order/direction of results from `Block3d.vertices` and
`Block3d.edges` will change.
-}
scaleAbout : Point3d units coordinates -> Float -> Block3d units coordinates -> Block3d units coordinates
scaleAbout point scale block =
    let
        currentFrame =
            axes block

        currentXDirection =
            Frame3d.xDirection currentFrame

        currentYDirection =
            Frame3d.yDirection currentFrame

        currentZDirection =
            Frame3d.zDirection currentFrame

        newCenterPoint =
            Point3d.scaleAbout point scale (Frame3d.originPoint currentFrame)

        newAxes =
            if scale >= 0 then
                Frame3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    , zDirection = currentZDirection
                    }

            else
                Frame3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction3d.reverse currentXDirection
                    , yDirection = Direction3d.reverse currentYDirection
                    , zDirection = Direction3d.reverse currentZDirection
                    }

        ( currentXDimension, currentYDimension, currentZDimension ) =
            dimensions block

        newXDimension =
            Quantity.abs (Quantity.multiplyBy scale currentXDimension)

        newYDimension =
            Quantity.abs (Quantity.multiplyBy scale currentYDimension)

        newZDimension =
            Quantity.abs (Quantity.multiplyBy scale currentZDimension)
    in
    Types.Block3d
        { axes = newAxes
        , dimensions = ( newXDimension, newYDimension, newZDimension )
        }


{-| Rotate a block around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Block3d units coordinates -> Block3d units coordinates
rotateAround axis angle block =
    Types.Block3d
        { axes = Frame3d.rotateAround axis angle (axes block)
        , dimensions = dimensions block
        }


{-| Translate a block by a given displacement.
-}
translateBy : Vector3d units coordinates -> Block3d units coordinates -> Block3d units coordinates
translateBy displacement block =
    Types.Block3d
        { axes = Frame3d.translateBy displacement (axes block)
        , dimensions = dimensions block
        }


{-| Translate a block in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Block3d units coordinates -> Block3d units coordinates
translateIn direction distance block =
    translateBy (Vector3d.withLength distance direction) block


{-| Mirror a block across a given plane. Note that this will flip the
handedness of the block's axes.
-}
mirrorAcross : Plane3d units coordinates -> Block3d units coordinates -> Block3d units coordinates
mirrorAcross plane block =
    Types.Block3d
        { axes = Frame3d.mirrorAcross plane (axes block)
        , dimensions = dimensions block
        }


{-| Take a block considered to be defined in local coordinates relative to a
given reference frame, and return that block expressed in global
coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Block3d units localCoordinates -> Block3d units globalCoordinates
placeIn frame block =
    Types.Block3d
        { axes = Frame3d.placeIn frame (axes block)
        , dimensions = dimensions block
        }


{-| Take a block defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Block3d units globalCoordinates -> Block3d units localCoordinates
relativeTo frame block =
    Types.Block3d
        { axes = Frame3d.relativeTo frame (axes block)
        , dimensions = dimensions block
        }


{-| Get the minimal bounding box containing a given block. This will have
exactly the same shape and size as the block itself if the block is
axis-aligned, but will be larger than the block if the block is at an
angle.

    cube =
        Block3d.from
            (Point3d.meters 0 0 0)
            (Point3d.meters 1 1 1)

    rotatedCube =
        cube
            |> Block3d.rotateAround Axis3d.y
                (Angle.degrees -45)

    Block3d.boundingBox rotatedCube
    --> BoundingBox3d.fromExtrema
    -->     { minX = Length.meters -0.7071
    -->     , maxX = Length.meters 0.7071
    -->     , minY = Length.meters 0
    -->     , maxY = Length.meters 1
    -->     , minZ = Length.meters 0
    -->     , maxZ = Length.meters 1.4142
    -->     }

-}
boundingBox : Block3d units coordinates -> BoundingBox3d units coordinates
boundingBox block =
    let
        frame =
            axes block

        p0 =
            Point3d.unwrap (Frame3d.originPoint frame)

        i =
            Direction3d.unwrap (Frame3d.xDirection frame)

        ix =
            abs i.x

        iy =
            abs i.y

        iz =
            abs i.z

        j =
            Direction3d.unwrap (Frame3d.yDirection frame)

        jx =
            abs j.x

        jy =
            abs j.y

        jz =
            abs j.z

        k =
            Direction3d.unwrap (Frame3d.zDirection frame)

        kx =
            abs k.x

        ky =
            abs k.y

        kz =
            abs k.z

        ( Quantity xDim, Quantity yDim, Quantity zDim ) =
            dimensions block

        halfXDim =
            xDim / 2

        halfYDim =
            yDim / 2

        halfZDim =
            zDim / 2

        minX =
            p0.x - ix * halfXDim - jx * halfYDim - kx * halfZDim

        maxX =
            p0.x + ix * halfXDim + jx * halfYDim + kx * halfZDim

        minY =
            p0.y - iy * halfXDim - jy * halfYDim - ky * halfZDim

        maxY =
            p0.y + iy * halfXDim + jy * halfYDim + ky * halfZDim

        minZ =
            p0.z - iz * halfXDim - jz * halfYDim - kz * halfZDim

        maxZ =
            p0.z + iz * halfXDim + jz * halfYDim + kz * halfZDim
    in
    Types.BoundingBox3d
        { minX = Quantity minX
        , maxX = Quantity maxX
        , minY = Quantity minY
        , maxY = Quantity maxY
        , minZ = Quantity minZ
        , maxZ = Quantity maxZ
        }


{-| Interpolate within a block based on coordinates which range from 0 to 1.
For example, the center point of a block is

    Block3d.interpolate block 0.5 0.5 0.5

-}
interpolate : Block3d units coordinates -> Float -> Float -> Float -> Point3d units coordinates
interpolate block u v w =
    let
        ( xDimension, yDimension, zDimension ) =
            dimensions block
    in
    Point3d.xyzIn (axes block)
        (Quantity.multiplyBy (u - 0.5) xDimension)
        (Quantity.multiplyBy (v - 0.5) yDimension)
        (Quantity.multiplyBy (w - 0.5) zDimension)
