module Cone3d exposing
    ( Cone3d
    , along, startingAt, from
    , axis, axialDirection, radius, diameter, length, basePoint
    , tipPoint, base, basePlane, volume, boundingBox
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , placeIn, relativeTo
    )

{-| A `Cone3d` consists of a conical outer surface, a circular base and a tip;
it is defined by its center point on the base, axial direction, radius and overall
length. This module contains functionality for:

  - Constructing cones in various ways
  - Scaling, rotating and translating cones
  - Extracting cone properties like base point and volume

@docs Cone3d


# Constructors

@docs along, startingAt, from


# Properties

@docs axis, axialDirection, radius, diameter, length, basePoint
@docs tipPoint, base, basePlane, volume, boundingBox


# Queries

@docs contains


# Transformations

Transformations generally behave just like [the ones in the
`Point3d` module](Point3d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs placeIn, relativeTo

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Cubed, Quantity, Rate)
import Vector3d exposing (Vector3d)


{-| -}
type alias Cone3d units coordinates =
    Types.Cone3d units coordinates


{-| Construct a cone that lies along the given axis, with the center of the base
and tip points given as (signed) distances along that axis:

    exampleCone =
        Cone3d.along Axis3d.x
            { base = Length.meters -1
            , tip = Length.meters 3
            , radius = Length.meters 1
            }

    Cone3d.basePoint exampleCone
    --> Point3d.meters -1 0 0

    Cone3d.tipPoint exampleCone
    --> Point3d.meters 3 0 0

    Cone3d.length exampleCone
    --> Length.meters 4

Note that `tip` may be less than `base`, but then the axial direction of the
returned cone will be the opposite of the axial direction of the given axis.

-}
along :
    Axis3d units coordinates
    -> { base : Quantity Float units, tip : Quantity Float units, radius : Quantity Float units }
    -> Cone3d units coordinates
along givenAxis arguments =
    let
        computedBasePoint =
            Point3d.along givenAxis arguments.base

        givenAxisDirection =
            Axis3d.direction givenAxis

        computedDirection =
            if arguments.tip |> Quantity.greaterThanOrEqualTo arguments.base then
                givenAxisDirection

            else
                Direction3d.reverse givenAxisDirection
    in
    Types.Cone3d
        { axis = Axis3d.through computedBasePoint computedDirection
        , radius = Quantity.abs arguments.radius
        , length = Quantity.abs (arguments.tip |> Quantity.minus arguments.base)
        }


{-| Construct a cone given the center point on the base, axial direction
towards the tip, radius and length. Negative values for radius or length
will be treated as positive (the absolute values will be used).
-}
startingAt :
    Point3d units coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float units, length : Quantity Float units }
    -> Cone3d units coordinates
startingAt givenBasePoint givenDirection arguments =
    Types.Cone3d
        { axis = Axis3d.through givenBasePoint givenDirection
        , radius = Quantity.abs arguments.radius
        , length = Quantity.abs arguments.length
        }


{-| Attempt to construct a cone from the given center point on the base,
tip point and radius. If the base and tip points are coincident (the same point),
returns `Nothing`.
-}
from : Point3d units coordinates -> Point3d units coordinates -> Quantity Float units -> Maybe (Cone3d units coordinates)
from givenBasePoint givenTipPoint givenRadius =
    case Direction3d.from givenBasePoint givenTipPoint of
        Just computedDirection ->
            Just <|
                Types.Cone3d
                    { axis = Axis3d.through givenBasePoint computedDirection
                    , radius = Quantity.abs givenRadius
                    , length = Point3d.distanceFrom givenBasePoint givenTipPoint
                    }

        Nothing ->
            Nothing


{-| Get the central axis of a cone. The origin point of the axis will be at
the center point of the base, and the direction of the axis will be from the
cone's base point towards its tip.
-}
axis : Cone3d units coordinates -> Axis3d units coordinates
axis (Types.Cone3d cone) =
    cone.axis


{-| Get the axial direction of a cone.
-}
axialDirection : Cone3d units coordinates -> Direction3d coordinates
axialDirection cone =
    Axis3d.direction (axis cone)


{-| Get the base radius of a cone.
-}
radius : Cone3d units coordinates -> Quantity Float units
radius (Types.Cone3d cone) =
    cone.radius


{-| Get the base diameter of a cone (twice its radius).
-}
diameter : Cone3d units coordinates -> Quantity Float units
diameter cone =
    Quantity.twice (radius cone)


{-| Get the overall length of a cone.
-}
length : Cone3d units coordinates -> Quantity Float units
length (Types.Cone3d cone) =
    cone.length


{-| Get the base point of a cone. This is the center point of the circle
that forms the base of the cone.
-}
basePoint : Cone3d units coordinates -> Point3d units coordinates
basePoint cone =
    Axis3d.originPoint (axis cone)


{-| Get the tip point of a cone.
-}
tipPoint : Cone3d units coordinates -> Point3d units coordinates
tipPoint cone =
    Point3d.along (axis cone) (length cone)


{-| Get the circle at the base of a cone. The axial direction of this
circle will be the _reverse_ of the axial direction of the cone itself (the
circle axial direction will point backwards/outwards).
-}
base : Cone3d units coordinates -> Circle3d units coordinates
base cone =
    Circle3d.withRadius (radius cone)
        (Direction3d.reverse (axialDirection cone))
        (basePoint cone)


{-| Get the plane at the base of a cone. The normal direction of this
plane will be the same as the axial direction of the cone itself.
-}
basePlane : Cone3d units coordinates -> Plane3d units coordinates
basePlane cone =
    Plane3d.through (basePoint cone) (axialDirection cone)


{-| Get the volume of a cone.
-}
volume : Cone3d units coordinates -> Quantity Float (Cubed units)
volume cone =
    Quantity.multiplyBy pi (Quantity.squared (radius cone))
        |> Quantity.times (length cone)
        |> Quantity.divideBy 3


{-| Get the minimal bounding box containing a given cone.
-}
boundingBox : Cone3d units coordinates -> BoundingBox3d units coordinates
boundingBox cone =
    BoundingBox3d.union
        (Circle3d.boundingBox (base cone))
        (BoundingBox3d.singleton (tipPoint cone))


{-| Check if a cone contains a given point.
-}
contains : Point3d units coordinates -> Cone3d units coordinates -> Bool
contains point cone =
    let
        coneAxis =
            axis cone

        axialDistance =
            Point3d.signedDistanceAlong coneAxis point

        radialDistance =
            Point3d.distanceFromAxis coneAxis point

        radiusAtPoint =
            Quantity.interpolateFrom (radius cone) Quantity.zero <|
                Quantity.ratio axialDistance (length cone)
    in
    (axialDistance |> Quantity.greaterThanOrEqualTo Quantity.zero)
        && (axialDistance |> Quantity.lessThanOrEqualTo (length cone))
        && (radialDistance |> Quantity.lessThanOrEqualTo radiusAtPoint)


{-| Scale a cone about a given point by a given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Cone3d units coordinates -> Cone3d units coordinates
scaleAbout point scale cone =
    let
        scaledBasePoint =
            Point3d.scaleAbout point scale (Axis3d.originPoint (axis cone))

        scaledDirection =
            if scale >= 0 then
                axialDirection cone

            else
                Direction3d.reverse (axialDirection cone)

        scaledRadius =
            Quantity.abs (Quantity.multiplyBy scale (radius cone))

        scaledLength =
            Quantity.abs (Quantity.multiplyBy scale (length cone))
    in
    Types.Cone3d
        { axis = Axis3d.through scaledBasePoint scaledDirection
        , radius = scaledRadius
        , length = scaledLength
        }


{-| Rotate a cone around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Cone3d units coordinates -> Cone3d units coordinates
rotateAround givenAxis givenAngle (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.rotateAround givenAxis givenAngle cone.axis
        , radius = cone.radius
        , length = cone.length
        }


{-| Translate a cone by a given displacement.
-}
translateBy : Vector3d units coordinates -> Cone3d units coordinates -> Cone3d units coordinates
translateBy displacement (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.translateBy displacement cone.axis
        , radius = cone.radius
        , length = cone.length
        }


{-| Translate a cone in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Cone3d units coordinates -> Cone3d units coordinates
translateIn givenDirection givenDistance (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.translateIn givenDirection givenDistance cone.axis
        , radius = cone.radius
        , length = cone.length
        }


{-| Mirror a cone across a given plane.
-}
mirrorAcross : Plane3d units coordinates -> Cone3d units coordinates -> Cone3d units coordinates
mirrorAcross plane (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.mirrorAcross plane cone.axis
        , radius = cone.radius
        , length = cone.length
        }


{-| Convert a cone from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Cone3d units1 coordinates -> Cone3d units2 coordinates
at rate (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.at rate cone.axis
        , radius = Quantity.abs (Quantity.at rate cone.radius)
        , length = Quantity.abs (Quantity.at rate cone.length)
        }


{-| Convert a cone from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Cone3d units1 coordinates -> Cone3d units2 coordinates
at_ rate (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.at_ rate cone.axis
        , radius = Quantity.abs (Quantity.at_ rate cone.radius)
        , length = Quantity.abs (Quantity.at_ rate cone.length)
        }


{-| Take a cone considered to be defined in local coordinates relative to a
given reference frame, and return that cone expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Cone3d units localCoordinates -> Cone3d units globalCoordinates
placeIn frame (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.placeIn frame cone.axis
        , radius = cone.radius
        , length = cone.length
        }


{-| Take a cone defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Cone3d units globalCoordinates -> Cone3d units localCoordinates
relativeTo frame (Types.Cone3d cone) =
    Types.Cone3d
        { axis = Axis3d.relativeTo frame cone.axis
        , radius = cone.radius
        , length = cone.length
        }
