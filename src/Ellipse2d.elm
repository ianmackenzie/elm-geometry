--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Ellipse2d exposing
    ( Ellipse2d
    , with
    , centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius, area
    , toEllipticalArc
    , boundingBox, signedDistanceAlong
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| An [ellipse](https://en.wikipedia.org/wiki/Ellipse) is defined by a center
point, X and Y radius, and X and Y axes (which will always be perpendicular to
each other). Ellipses are symmetric about their X and Y axes. This module
includes functionality for

  - Constructing ellipses
  - Scaling, rotating and translating ellipses
  - Converting ellipses between different coordinate systems

@docs Ellipse2d


# Constructors

@docs with


# Properties

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius, area


# Conversion

@docs toEllipticalArc


# Bounds

@docs boundingBox, signedDistanceAlong


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
import Geometry.Types as Types exposing (EllipticalArc2d(..))
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval exposing (Interval)
import Vector2d exposing (Vector2d)


{-| -}
type alias Ellipse2d units coordinates =
    Types.Ellipse2d units coordinates


{-| Construct an ellipse from its center point, X direction, and X and Y radii.
If you pass a negative radius, the absolute value will be used.

    exampleEllipse =
        Ellipse2d.with
            { centerPoint = Point2d.meters 10 10
            , xDirection = Direction2d.degrees 30
            , xRadius = Length.meters 5
            , yRadius = Length.meters 3
            }

-}
with :
    { centerPoint : Point2d units coordinates
    , xDirection : Direction2d coordinates
    , xRadius : Quantity Float units
    , yRadius : Quantity Float units
    }
    -> Ellipse2d units coordinates
with properties =
    Types.Ellipse2d
        { axes =
            Frame2d.withXDirection properties.xDirection properties.centerPoint
        , xRadius = Quantity.abs properties.xRadius
        , yRadius = Quantity.abs properties.yRadius
        }


{-| Convert an ellipse from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Ellipse2d units1 coordinates -> Ellipse2d units2 coordinates
at rate (Types.Ellipse2d ellipse) =
    Types.Ellipse2d
        { axes = Frame2d.at rate ellipse.axes
        , xRadius = Quantity.abs (Quantity.at rate ellipse.xRadius)
        , yRadius = Quantity.abs (Quantity.at rate ellipse.yRadius)
        }


{-| Convert an ellipse from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Ellipse2d units1 coordinates -> Ellipse2d units2 coordinates
at_ rate ellipse =
    at (Quantity.inverse rate) ellipse


{-| Get the center point of an ellipse.
-}
centerPoint : Ellipse2d units coordinates -> Point2d units coordinates
centerPoint ellipse =
    Frame2d.originPoint (axes ellipse)


{-| Get the X and Y axes of an ellipse as a `Frame2d`.

    Ellipse2d.axes exampleEllipse
    --> Frame2d.withXDirection (Direction2d.degrees 30)
    -->     (Point2d.meters 10 10)

-}
axes : Ellipse2d units coordinates -> Frame2d units coordinates defines
axes (Types.Ellipse2d ellipse) =
    Frame2d.copy ellipse.axes


{-| Get the X axis of an ellipse.
-}
xAxis : Ellipse2d units coordinates -> Axis2d units coordinates
xAxis ellipse =
    Frame2d.xAxis (axes ellipse)


{-| Get the Y axis of an ellipse.
-}
yAxis : Ellipse2d units coordinates -> Axis2d units coordinates
yAxis ellipse =
    Frame2d.yAxis (axes ellipse)


{-| Get the radius of an ellipse along its X axis. This may be either the
minimum or maximum radius.

    Ellipse2d.xRadius exampleEllipse
    --> Length.meters 5

-}
xRadius : Ellipse2d units coordinates -> Quantity Float units
xRadius (Types.Ellipse2d ellipse) =
    ellipse.xRadius


{-| Get the radius of an ellipse along its Y axis. This may be either the
minimum or maximum radius.

    Ellipse2d.yRadius exampleEllipse
    --> Length.meters 3

-}
yRadius : Ellipse2d units coordinates -> Quantity Float units
yRadius (Types.Ellipse2d ellipse) =
    ellipse.yRadius


{-| Get the direction of the ellipse's X axis.
-}
xDirection : Ellipse2d units coordinates -> Direction2d coordinates
xDirection ellipse =
    Frame2d.xDirection (axes ellipse)


{-| Get the direction of an ellipse's Y axis.
-}
yDirection : Ellipse2d units coordinates -> Direction2d coordinates
yDirection ellipse =
    Frame2d.yDirection (axes ellipse)


{-| Get the area of an ellipse.

    Ellipse2d.area exampleEllipse
    --> Area.squareMeters 47.1239

-}
area : Ellipse2d units coordinates -> Quantity Float (Squared units)
area ellipse =
    Quantity.multiplyBy pi (xRadius ellipse |> Quantity.times (yRadius ellipse))


{-| Convert an ellipse to a 360 degree elliptical arc.
-}
toEllipticalArc : Ellipse2d units coordinates -> EllipticalArc2d units coordinates
toEllipticalArc ellipse =
    EllipticalArc2d
        { ellipse = ellipse
        , startAngle = Quantity.zero
        , sweptAngle = Angle.turns 1
        }


{-| Get the minimal bounding box containing a given ellipse.
-}
boundingBox : Ellipse2d units coordinates -> BoundingBox2d units coordinates
boundingBox ellipse =
    BoundingBox2d.xy
        (signedDistanceAlong Axis2d.x ellipse)
        (signedDistanceAlong Axis2d.y ellipse)


{-| Project an ellipse onto an axis, returning the range of projected distances
along that axis.
-}
signedDistanceAlong : Axis2d units coordinates -> Ellipse2d units coordinates -> Interval Float units
signedDistanceAlong axis ellipse =
    let
        centralDistance =
            Point2d.signedDistanceAlong axis (centerPoint ellipse)

        axisDirection =
            Axis2d.direction axis

        a =
            xRadius ellipse
                |> Quantity.multiplyBy
                    (Direction2d.componentIn axisDirection (xDirection ellipse))

        b =
            yRadius ellipse
                |> Quantity.multiplyBy
                    (Direction2d.componentIn axisDirection (yDirection ellipse))

        delta =
            Quantity.squared a
                |> Quantity.plus (Quantity.squared b)
                |> Quantity.sqrt
    in
    Interval.from
        (centralDistance |> Quantity.minus delta)
        (centralDistance |> Quantity.plus delta)


{-| Scale an ellipse about a given point by a given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> Ellipse2d units coordinates -> Ellipse2d units coordinates
scaleAbout point scale ellipse =
    let
        newCenterPoint =
            Point2d.scaleAbout point scale (centerPoint ellipse)

        newAxes =
            if scale >= 0 then
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = xDirection ellipse
                    , yDirection = yDirection ellipse
                    }

            else
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction2d.reverse (xDirection ellipse)
                    , yDirection = Direction2d.reverse (yDirection ellipse)
                    }
    in
    Types.Ellipse2d
        { axes = newAxes
        , xRadius = Quantity.abs (Quantity.multiplyBy scale (xRadius ellipse))
        , yRadius = Quantity.abs (Quantity.multiplyBy scale (yRadius ellipse))
        }


transformBy : (Frame2d units coordinates1 {} -> Frame2d units coordinates2 {}) -> Ellipse2d units coordinates1 -> Ellipse2d units coordinates2
transformBy axesTransformation (Types.Ellipse2d properties) =
    Types.Ellipse2d
        { axes = axesTransformation properties.axes
        , xRadius = properties.xRadius
        , yRadius = properties.yRadius
        }


{-| Rotate an ellipse around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Ellipse2d units coordinates -> Ellipse2d units coordinates
rotateAround point angle ellipse =
    transformBy (Frame2d.rotateAround point angle) ellipse


{-| Translate an ellipse by a given displacement.
-}
translateBy : Vector2d units coordinates -> Ellipse2d units coordinates -> Ellipse2d units coordinates
translateBy displacement ellipse =
    transformBy (Frame2d.translateBy displacement) ellipse


{-| Translate an ellipse in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Ellipse2d units coordinates -> Ellipse2d units coordinates
translateIn direction distance ellipse =
    translateBy (Vector2d.withLength distance direction) ellipse


{-| Mirror an ellipse across a given axis. Note that if the axes of the original
ellipse form a [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
frame, then the axes of the mirrored ellipse will form a left-handed frame (and
vice versa).
-}
mirrorAcross : Axis2d units coordinates -> Ellipse2d units coordinates -> Ellipse2d units coordinates
mirrorAcross axis ellipse =
    transformBy (Frame2d.mirrorAcross axis) ellipse


{-| Take an ellipse defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Ellipse2d units globalCoordinates -> Ellipse2d units localCoordinates
relativeTo frame ellipse =
    transformBy (Frame2d.relativeTo frame) ellipse


{-| Take an ellipse considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Ellipse2d units localCoordinates -> Ellipse2d units globalCoordinates
placeIn frame ellipse =
    transformBy (Frame2d.placeIn frame) ellipse
