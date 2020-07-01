--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Ellipse3d exposing
    ( Ellipse3d
    , on
    , centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius, area
    , toEllipticalArc
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| An `Ellipse3d` is an `Ellipse2d` in 3D space. This module contains
functionality for:

  - Constructing ellipses
  - Scaling, rotating and translating ellipses
  - Converting ellipses between different coordinate systems

@docs Ellipse3d


# Constructors

@docs on


# Properties

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius, area


# Conversion

@docs toEllipticalArc


# Transformations

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
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (EllipticalArc3d(..))
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Extra as Quantity
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Ellipse3d units coordinates =
    Types.Ellipse3d units coordinates


{-| Construct a 3D ellipse by placing a 2D ellipse on a sketch plane.
-}
on :
    SketchPlane3d units coordinates { defines : coordinates2d }
    -> Ellipse2d units coordinates2d
    -> Ellipse3d units coordinates
on sketchPlane ellipse2d =
    Types.Ellipse3d
        { axes = SketchPlane3d.on sketchPlane (Ellipse2d.axes ellipse2d)
        , xRadius = Ellipse2d.xRadius ellipse2d
        , yRadius = Ellipse2d.yRadius ellipse2d
        }


{-| Convert an ellipse from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Ellipse3d units1 coordinates -> Ellipse3d units2 coordinates
at rate (Types.Ellipse3d ellipse) =
    Types.Ellipse3d
        { axes = SketchPlane3d.at rate ellipse.axes
        , xRadius = Quantity.abs (Quantity.at rate ellipse.xRadius)
        , yRadius = Quantity.abs (Quantity.at rate ellipse.yRadius)
        }


{-| Convert an ellipse from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Ellipse3d units1 coordinates -> Ellipse3d units2 coordinates
at_ rate ellipse =
    at (Quantity.inverse rate) ellipse


{-| Get the center point of an ellipse.
-}
centerPoint : Ellipse3d units coordinates -> Point3d units coordinates
centerPoint ellipse =
    SketchPlane3d.originPoint (axes ellipse)


{-| Get the X and Y axes of an ellipse as a `SketchPlane3d`.
-}
axes : Ellipse3d units coordinates -> SketchPlane3d units coordinates defines
axes (Types.Ellipse3d ellipse) =
    SketchPlane3d.copy ellipse.axes


{-| Get the X axis of an ellipse.
-}
xAxis : Ellipse3d units coordinates -> Axis3d units coordinates
xAxis ellipse =
    SketchPlane3d.xAxis (axes ellipse)


{-| Get the Y axis of an ellipse.
-}
yAxis : Ellipse3d units coordinates -> Axis3d units coordinates
yAxis ellipse =
    SketchPlane3d.yAxis (axes ellipse)


{-| Get the radius of an ellipse along its X axis. This may be either the
minimum or maximum radius.
-}
xRadius : Ellipse3d units coordinates -> Quantity Float units
xRadius (Types.Ellipse3d ellipse) =
    ellipse.xRadius


{-| Get the radius of an ellipse along its Y axis. This may be either the
minimum or maximum radius.
-}
yRadius : Ellipse3d units coordinates -> Quantity Float units
yRadius (Types.Ellipse3d ellipse) =
    ellipse.yRadius


{-| Get the direction of the ellipse's X axis.
-}
xDirection : Ellipse3d units coordinates -> Direction3d coordinates
xDirection ellipse =
    SketchPlane3d.xDirection (axes ellipse)


{-| Get the direction of an ellipse's Y axis.
-}
yDirection : Ellipse3d units coordinates -> Direction3d coordinates
yDirection ellipse =
    SketchPlane3d.yDirection (axes ellipse)


{-| Get the area of an ellipse.
-}
area : Ellipse3d units coordinates -> Quantity Float (Squared units)
area ellipse =
    Quantity.multiplyBy pi (xRadius ellipse |> Quantity.times (yRadius ellipse))


{-| Convert an ellipse to a 360 degree elliptical arc.
-}
toEllipticalArc : Ellipse3d units coordinates -> EllipticalArc3d units coordinates
toEllipticalArc ellipse =
    Types.EllipticalArc3d
        { ellipse = ellipse
        , startAngle = Quantity.zero
        , sweptAngle = Angle.turns 1
        }


{-| Scale an ellipse about a given point by a given scale.
-}
scaleAbout :
    Point3d units coordinates
    -> Float
    -> Ellipse3d units coordinates
    -> Ellipse3d units coordinates
scaleAbout point scale ellipse =
    let
        newCenterPoint =
            Point3d.scaleAbout point scale (centerPoint ellipse)

        newAxes =
            if scale >= 0 then
                SketchPlane3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = xDirection ellipse
                    , yDirection = yDirection ellipse
                    }

            else
                SketchPlane3d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction3d.reverse (xDirection ellipse)
                    , yDirection = Direction3d.reverse (yDirection ellipse)
                    }
    in
    Types.Ellipse3d
        { axes = newAxes
        , xRadius = Quantity.abs (Quantity.multiplyBy scale (xRadius ellipse))
        , yRadius = Quantity.abs (Quantity.multiplyBy scale (yRadius ellipse))
        }


transformBy :
    (SketchPlane3d units coordinates1 {} -> SketchPlane3d units coordinates2 {})
    -> Ellipse3d units coordinates1
    -> Ellipse3d units coordinates2
transformBy axesTransformation (Types.Ellipse3d properties) =
    Types.Ellipse3d
        { axes = axesTransformation properties.axes
        , xRadius = properties.xRadius
        , yRadius = properties.yRadius
        }


{-| Rotate an ellipse around a given axis by a given angle.
-}
rotateAround :
    Axis3d units coordinates
    -> Angle
    -> Ellipse3d units coordinates
    -> Ellipse3d units coordinates
rotateAround axis angle ellipse =
    transformBy (SketchPlane3d.rotateAround axis angle) ellipse


{-| Translate an ellipse by a given displacement.
-}
translateBy :
    Vector3d units coordinates
    -> Ellipse3d units coordinates
    -> Ellipse3d units coordinates
translateBy displacement ellipse =
    transformBy (SketchPlane3d.translateBy displacement) ellipse


{-| Translate an ellipse in a given direction by a given distance.
-}
translateIn :
    Direction3d coordinates
    -> Quantity Float units
    -> Ellipse3d units coordinates
    -> Ellipse3d units coordinates
translateIn direction distance ellipse =
    translateBy (Vector3d.withLength distance direction) ellipse


{-| Mirror an ellipse across a given plane.
-}
mirrorAcross :
    Plane3d units coordinates
    -> Ellipse3d units coordinates
    -> Ellipse3d units coordinates
mirrorAcross plane ellipse =
    transformBy (SketchPlane3d.mirrorAcross plane) ellipse


{-| Take an ellipse defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipse3d units globalCoordinates
    -> Ellipse3d units localCoordinates
relativeTo frame ellipse =
    transformBy (SketchPlane3d.relativeTo frame) ellipse


{-| Take an ellipse considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipse3d units localCoordinates
    -> Ellipse3d units globalCoordinates
placeIn frame ellipse =
    transformBy (SketchPlane3d.placeIn frame) ellipse
