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

{-| Docs here

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
import Vector3d exposing (Vector3d)


{-| -}
type alias Ellipsoid3d units coordinates =
    Types.Ellipsoid3d units coordinates


{-| Construct a 3d ellipsoid from a [Frame3d](Frame3d) and the x, y and z
radii. If you pass a negative radius, the absolute value will be used.

    exampleEllipsoid =
        Ellipsoid3d.with
            { axes = Frame3d.atPoint (Point3d.meters 2 1 3)
            , xRadius = Length.meters 5
            , yRadius = Length.meters 3
            , zRadius = Length.meters 4
            }

-}
with :
    { axes : Frame3d units coordinates {}
    , xRadius : Quantity Float units
    , yRadius : Quantity Float units
    , zRadius : Quantity Float units
    }
    -> Ellipsoid3d units coordinates
with properties =
    Types.Ellipsoid3d
        { axes = properties.axes
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
boundingBox _ =
    Debug.todo "boundingBox"


{-| -}
contains : Point3d units coordinates -> Ellipsoid3d units coordinates -> Bool
contains _ =
    Debug.todo "boundingBox"


{-| -}
signedDistanceAlong : Axis3d units coordinates -> Ellipsoid3d units coordinates -> Quantity Float units
signedDistanceAlong _ _ =
    Debug.todo "signedDistanceAlong"


{-| -}
scaleAbout :
    Point3d units coordinates
    -> Float
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
scaleAbout _ _ _ =
    Debug.todo "scaleAbout"


{-| -}
rotateAround :
    Axis3d units coordinates
    -> Angle
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
rotateAround _ _ _ =
    Debug.todo "rotateAround"


{-| -}
translateBy :
    Vector3d units coordinates
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
translateBy _ _ =
    Debug.todo "translateBy"


{-| -}
translateIn :
    Direction3d coordinates
    -> Quantity Float units
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
translateIn _ _ _ =
    Debug.todo "translateIn"


{-| -}
mirrorAcross :
    Plane3d units coordinates
    -> Ellipsoid3d units coordinates
    -> Ellipsoid3d units coordinates
mirrorAcross _ _ =
    Debug.todo "mirrorAcross"


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


{-| -}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipsoid3d units globalCoordinates
    -> Ellipsoid3d units localCoordinates
relativeTo _ _ =
    Debug.todo "relativeTo"


{-| -}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Ellipsoid3d units localCoordinates
    -> Ellipsoid3d units globalCoordinates
placeIn _ _ =
    Debug.todo "placeIn"
