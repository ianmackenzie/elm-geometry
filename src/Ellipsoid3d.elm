--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Ellipsoid3d exposing
    ( Ellipsoid3d
    , with
    , centerPoint, axes, xAxis, yAxis, zAxis, xDirection, yDirection, zDirection, xRadius, yRadius, zRadius, volume, boundingBox
    , contains
    , signedDistanceAlong
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| An [ellipsoid](https://en.wikipedia.org/wiki/Ellipsoid) is defined by a
center point, X, Y and Z radii, and a `Frame3d` representing the ellipsoid's
axes. This module includes functionality for

  - Constructing ellipsoids
  - Scaling, rotating and translating ellipsoids
  - Converting ellipsoids between different coordinate systems

@docs Ellipsoid3d


# Constructors

@docs with


# Properties

@docs centerPoint, axes, xAxis, yAxis, zAxis, xDirection, yDirection, zDirection, xRadius, yRadius, zRadius, volume, boundingBox


# Queries

@docs contains


# Measurement

@docs signedDistanceAlong


# Transformations

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
import Geometry.Types as Types exposing (Ellipsoid3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Cubed, Quantity, Rate, Squared)
import Quantity.Interval as Interval exposing (Interval)
import Vector3d exposing (Vector3d)


{-| -}
type alias Ellipsoid3d units coordinates =
    Types.Ellipsoid3d units coordinates


{-| Construct a 3d ellipsoid from its axes and X, Y and Z radii. If you pass a
negative radius, the absolute value will be used.

    exampleEllipsoid =
        Ellipsoid3d.with
            { axes = Frame3d.atPoint (Point3d.meters 2 1 3)
            , xRadius = Length.meters 5
            , yRadius = Length.meters 3
            , zRadius = Length.meters 4
            }

-}
with :
    { axes : Frame3d units coordinates defines
    , xRadius : Quantity Float units
    , yRadius : Quantity Float units
    , zRadius : Quantity Float units
    }
    -> Ellipsoid3d units coordinates
with properties =
    Types.Ellipsoid3d
        { axes = Frame3d.copy properties.axes
        , xRadius = Quantity.abs properties.xRadius
        , yRadius = Quantity.abs properties.yRadius
        , zRadius = Quantity.abs properties.zRadius
        }


{-| Get the geometric center of an ellipsoid.
-}
centerPoint : Ellipsoid3d units coordinates -> Point3d units coordinates
centerPoint ellipsoid =
    Frame3d.originPoint (axes ellipsoid)


{-| Get the X, Y and Z axes of an ellipsoid as a `Frame3d`.
-}
axes : Ellipsoid3d units coordinates -> Frame3d units coordinates defines
axes (Types.Ellipsoid3d ellipsoid) =
    Frame3d.copy ellipsoid.axes


{-| Get the X axis of an ellipsoid.
-}
xAxis : Ellipsoid3d units coordinates -> Axis3d units coordinates
xAxis ellipsoid =
    Frame3d.xAxis (axes ellipsoid)


{-| Get the Y axis of an ellipsoid.
-}
yAxis : Ellipsoid3d units coordinates -> Axis3d units coordinates
yAxis ellipsoid =
    Frame3d.yAxis (axes ellipsoid)


{-| Get the Z axis of an ellipsoid.
-}
zAxis : Ellipsoid3d units coordinates -> Axis3d units coordinates
zAxis ellipsoid =
    Frame3d.zAxis (axes ellipsoid)


{-| Get the radius of an ellipsoid along its X axis.

    Ellipsoid3d.xRadius exampleEllipsoid
    --> Length.meters 5

-}
xRadius : Ellipsoid3d units coordinates -> Quantity Float units
xRadius (Types.Ellipsoid3d ellipsoid) =
    ellipsoid.xRadius


{-| Get the radius of an ellipsoid along its Y axis.

    Ellipsoid3d.yRadius exampleEllipsoid
    --> Length.meters 3

-}
yRadius : Ellipsoid3d units coordinates -> Quantity Float units
yRadius (Types.Ellipsoid3d ellipsoid) =
    ellipsoid.yRadius


{-| Get the radius of an ellipsoid along its Z axis.

    Ellipsoid3d.zRadius exampleEllipsoid
    --> Length.meters 4

-}
zRadius : Ellipsoid3d units coordinates -> Quantity Float units
zRadius (Types.Ellipsoid3d ellipsoid) =
    ellipsoid.zRadius


{-| Get the direction of the ellipsoid's X axis.
-}
xDirection : Ellipsoid3d units coordinates -> Direction3d coordinates
xDirection ellipsoid =
    Frame3d.xDirection (axes ellipsoid)


{-| Get the direction of the ellipsoid's Y axis.
-}
yDirection : Ellipsoid3d units coordinates -> Direction3d coordinates
yDirection ellipsoid =
    Frame3d.yDirection (axes ellipsoid)


{-| Get the direction of the ellipsoid's Z axis.
-}
zDirection : Ellipsoid3d units coordinates -> Direction3d coordinates
zDirection ellipsoid =
    Frame3d.zDirection (axes ellipsoid)


{-| Get the volume of an ellipsoid.
-}
volume : Ellipsoid3d units coordinates -> Quantity Float (Cubed units)
volume ellipsoid =
    let
        axesProduct =
            xRadius ellipsoid
                |> Quantity.times (yRadius ellipsoid)
                |> Quantity.times (zRadius ellipsoid)
    in
    Quantity.multiplyBy (4 / 3 * pi) axesProduct


{-| Get the minimal bounding box containing a given ellipsoid.

    Ellipsoid3d.boundingBox exampleSphere
    --> BoundingBox3d.from
    -->     (Point3d.meters -3 -2 -1)
    -->     (Point3d.meters 7 4 7)

-}
boundingBox : Ellipsoid3d units coordinates -> BoundingBox3d units coordinates
boundingBox ellipsoid =
    BoundingBox3d.xyz
        (signedDistanceAlong Axis3d.x ellipsoid)
        (signedDistanceAlong Axis3d.y ellipsoid)
        (signedDistanceAlong Axis3d.z ellipsoid)


{-| Check if an ellipsoid contains a given point.
-}
contains : Point3d units coordinates -> Ellipsoid3d units coordinates -> Bool
contains point ellipsoid =
    let
        localFrame =
            axes ellipsoid

        x =
            Point3d.xCoordinateIn localFrame point

        y =
            Point3d.yCoordinateIn localFrame point

        z =
            Point3d.zCoordinateIn localFrame point

        c1 =
            Quantity.squared (xRadius ellipsoid)
                |> Quantity.ratio (Quantity.squared x)

        c2 =
            Quantity.squared (yRadius ellipsoid)
                |> Quantity.ratio (Quantity.squared y)

        c3 =
            Quantity.squared (zRadius ellipsoid)
                |> Quantity.ratio (Quantity.squared z)
    in
    c1 + c2 + c3 <= 1


{-| Project an ellipsoid onto an axis, returning the range of projected distances
along that axis.
-}
signedDistanceAlong : Axis3d units coordinates -> Ellipsoid3d units coordinates -> Interval Float units
signedDistanceAlong axis ellipsoid =
    let
        centralDistance =
            Point3d.signedDistanceAlong axis (centerPoint ellipsoid)

        axisDirection =
            Axis3d.direction axis

        a =
            xRadius ellipsoid
                |> Quantity.multiplyBy
                    (Direction3d.componentIn axisDirection (xDirection ellipsoid))

        b =
            yRadius ellipsoid
                |> Quantity.multiplyBy
                    (Direction3d.componentIn axisDirection (yDirection ellipsoid))

        c =
            zRadius ellipsoid
                |> Quantity.multiplyBy
                    (Direction3d.componentIn axisDirection (zDirection ellipsoid))

        delta =
            Quantity.squared a
                |> Quantity.plus (Quantity.squared b)
                |> Quantity.plus (Quantity.squared c)
                |> Quantity.sqrt
    in
    Interval.from
        (centralDistance |> Quantity.minus delta)
        (centralDistance |> Quantity.plus delta)


{-| Scale an ellipsoid about a given point by a given scale.
-}
scaleAbout :
    Point3d units coordinates
    -> Float
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
scaleAbout point scale ellipsoid =
    let
        newCenterPoint =
            Point3d.scaleAbout point scale (centerPoint ellipsoid)

        newAxes =
            if scale >= 0 then
                Frame3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = xDirection ellipsoid
                    , yDirection = yDirection ellipsoid
                    , zDirection = yDirection ellipsoid
                    }

            else
                Frame3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction3d.reverse (xDirection ellipsoid)
                    , yDirection = Direction3d.reverse (yDirection ellipsoid)
                    , zDirection = Direction3d.reverse (yDirection ellipsoid)
                    }
    in
    Types.Ellipsoid3d
        { axes = newAxes
        , xRadius = Quantity.abs (Quantity.multiplyBy scale (xRadius ellipsoid))
        , yRadius = Quantity.abs (Quantity.multiplyBy scale (yRadius ellipsoid))
        , zRadius = Quantity.abs (Quantity.multiplyBy scale (zRadius ellipsoid))
        }


transformBy :
    (Frame3d units coordinates1 {} -> Frame3d units coordinates2 {})
    -> Ellipsoid3d units coordinates1
    -> Ellipsoid3d units coordinates2
transformBy axesTransformation (Types.Ellipsoid3d properties) =
    Types.Ellipsoid3d
        { axes = axesTransformation properties.axes
        , xRadius = properties.xRadius
        , yRadius = properties.yRadius
        , zRadius = properties.zRadius
        }


{-| Rotate an ellipsoid around a given axis by a given angle.
-}
rotateAround :
    Axis3d units coordinates
    -> Angle
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
rotateAround axis angle ellipsoid =
    transformBy (Frame3d.rotateAround axis angle) ellipsoid


{-| Translate an ellipsoid by a given displacement.
-}
translateBy :
    Vector3d units coordinates
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
translateBy displacement ellipsoid =
    transformBy (Frame3d.translateBy displacement) ellipsoid


{-| Translate an ellipsoid in a given direction by a given distance.
-}
translateIn :
    Direction3d coordinates
    -> Quantity Float units
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
translateIn direction distance ellipsoid =
    translateBy (Vector3d.withLength distance direction) ellipsoid


{-| Mirror an ellipsoid across a given plane. Note that this will flip the
handedness of the ellipsoid's axes.
-}
mirrorAcross :
    Plane3d units coordinates
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
mirrorAcross plane ellipsoid =
    transformBy (Frame3d.mirrorAcross plane) ellipsoid


{-| Convert an ellipsoid from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect
to source units.
-}
at :
    Quantity Float (Rate units2 units1)
    -> Ellipsoid3d units1 coordinates
    -> Ellipsoid3d units2 coordinates
at rate (Types.Ellipsoid3d ellipsoid) =
    Types.Ellipsoid3d
        { axes = Frame3d.at rate ellipsoid.axes
        , xRadius = Quantity.abs (Quantity.at rate ellipsoid.xRadius)
        , yRadius = Quantity.abs (Quantity.at rate ellipsoid.yRadius)
        , zRadius = Quantity.abs (Quantity.at rate ellipsoid.zRadius)
        }


{-| Convert an ellipsoid from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ :
    Quantity Float (Rate units1 units2)
    -> Ellipsoid3d units1 coordinates
    -> Ellipsoid3d units2 coordinates
at_ rate ellipsoid =
    at (Quantity.inverse rate) ellipsoid


{-| Take an ellipsoid defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipsoid3d units globalCoordinates
    -> Ellipsoid3d units localCoordinates
relativeTo frame ellipsoid =
    transformBy (Frame3d.relativeTo frame) ellipsoid


{-| Take an ellipsoid considered to be defined in local coordinates relative to a
given reference frame, and return that ellipsoid expressed in global coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipsoid3d units localCoordinates
    -> Ellipsoid3d units globalCoordinates
placeIn frame ellipsoid =
    transformBy (Frame3d.placeIn frame) ellipsoid
