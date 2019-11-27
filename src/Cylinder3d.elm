module Cylinder3d exposing
    ( Cylinder3d
    , along, centeredOn, startingAt, from
    , axis, centerPoint, axialDirection, radius, diameter, length, startPoint
    , endPoint, startCap, endCap, volume, boundingBox
    , contains
    , reverse
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , placeIn, relativeTo
    )

{-| A `Cylinder3d` consists of a cylindrical outer surface and two circular
end caps; it is defined by its center point, axial direction, radius and overall
length. This module contains functionality for:

  - Constructing cylinders in various ways
  - Scaling, rotating and translating cylinders
  - Extracting cylinder properties like center point and volume

@docs Cylinder3d


# Constructors

@docs along, centeredOn, startingAt, from


# Properties

@docs axis, centerPoint, axialDirection, radius, diameter, length, startPoint
@docs endPoint, startCap, endCap, volume, boundingBox


# Queries

@docs contains


# Transformations

@docs reverse

The remaining transformations generally behave just like [the ones in the
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
type alias Cylinder3d units coordinates =
    Types.Cylinder3d units coordinates


{-| Construct a cylinder that lies along the given axis, with the start and end
points given as (signed) distances along that axis:

    exampleCylinder =
        Cylinder3d.along Axis3d.x
            { start = Length.meters -1
            , end = Length.meters 3
            , radius = Length.meters 1
            }

    Cylinder3d.startPoint exampleCylinder
    --> Point3d.meters -1 0 0

    Cylinder3d.endPoint exampleCylinder
    --> Point3d.meters 3 0 0

    Cylinder3d.length exampleCylinder
    --> Length.meters 4

Note that `end` may be less than `start`, but then the axial direction of the
returned cylinder will be the opposite of the axial direction of the given axis.

-}
along :
    Axis3d units coordinates
    -> { start : Quantity Float units, end : Quantity Float units, radius : Quantity Float units }
    -> Cylinder3d units coordinates
along givenAxis arguments =
    let
        computedCenterPoint =
            Point3d.along givenAxis (Quantity.midpoint arguments.start arguments.end)

        givenAxisDirection =
            Axis3d.direction givenAxis

        computedDirection =
            if arguments.end |> Quantity.greaterThanOrEqualTo arguments.start then
                givenAxisDirection

            else
                Direction3d.reverse givenAxisDirection
    in
    Types.Cylinder3d
        { axis = Axis3d.through computedCenterPoint computedDirection
        , radius = Quantity.abs arguments.radius
        , length = Quantity.abs (arguments.end |> Quantity.minus arguments.start)
        }


{-| Construct a cylinder given its center point, axial direction, radius and
length. Negative values for radius or length will be treated as positive (the
absolute values will be used).
-}
centeredOn :
    Point3d units coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float units, length : Quantity Float units }
    -> Cylinder3d units coordinates
centeredOn givenCenterPoint givenDirection arguments =
    Types.Cylinder3d
        { axis = Axis3d.through givenCenterPoint givenDirection
        , radius = Quantity.abs arguments.radius
        , length = Quantity.abs arguments.length
        }


{-| Construct a cylinder given its start point, axial direction, radius and
length. Negative values for radius or length will be treated as positive (the
absolute values will be used).
-}
startingAt :
    Point3d units coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float units, length : Quantity Float units }
    -> Cylinder3d units coordinates
startingAt givenStartPoint givenDirection arguments =
    let
        computedLength =
            Quantity.abs arguments.length

        computedCenterPoint =
            givenStartPoint |> Point3d.translateIn givenDirection (Quantity.half computedLength)
    in
    Types.Cylinder3d
        { axis = Axis3d.through computedCenterPoint givenDirection
        , radius = Quantity.abs arguments.radius
        , length = computedLength
        }


{-| Attempt to construct a cylinder from the given start point, end point and
radius. If the start and end points are coincident (the same point), returns
`Nothing`.
-}
from : Point3d units coordinates -> Point3d units coordinates -> Quantity Float units -> Maybe (Cylinder3d units coordinates)
from givenStartPoint givenEndPoint givenRadius =
    case Direction3d.from givenStartPoint givenEndPoint of
        Just computedDirection ->
            let
                computedCenterPoint =
                    Point3d.midpoint givenStartPoint givenEndPoint
            in
            Just <|
                Types.Cylinder3d
                    { axis = Axis3d.through computedCenterPoint computedDirection
                    , radius = Quantity.abs givenRadius
                    , length = Point3d.distanceFrom givenStartPoint givenEndPoint
                    }

        Nothing ->
            Nothing


{-| Get the central axis of a cylinder. The origin point of the axis will be at
the center point of the cylinder, and the direction of the axis will be from the
cylinder's start point towards its end point.
-}
axis : Cylinder3d units coordinates -> Axis3d units coordinates
axis (Types.Cylinder3d cylinder) =
    cylinder.axis


{-| Get the center point of a cylinder.
-}
centerPoint : Cylinder3d units coordinates -> Point3d units coordinates
centerPoint cylinder =
    Axis3d.originPoint (axis cylinder)


{-| Get the axial direction of a cylinder.
-}
axialDirection : Cylinder3d units coordinates -> Direction3d coordinates
axialDirection cylinder =
    Axis3d.direction (axis cylinder)


{-| Get the radius of a cylinder.
-}
radius : Cylinder3d units coordinates -> Quantity Float units
radius (Types.Cylinder3d cylinder) =
    cylinder.radius


{-| Get the diameter of a cylinder (twice its radius).
-}
diameter : Cylinder3d units coordinates -> Quantity Float units
diameter cylinder =
    Quantity.twice (radius cylinder)


{-| Get the overall length of a cylinder.
-}
length : Cylinder3d units coordinates -> Quantity Float units
length (Types.Cylinder3d cylinder) =
    cylinder.length


{-| Get the start point of a cylinder. This is the center point of the circle
that forms the start cap of the cylinder.
-}
startPoint : Cylinder3d units coordinates -> Point3d units coordinates
startPoint cylinder =
    Point3d.along (axis cylinder) (Quantity.multiplyBy -0.5 (length cylinder))


{-| Get the end point of a cylinder. This is the center point of the circle that
forms the end cap of the cylinder.
-}
endPoint : Cylinder3d units coordinates -> Point3d units coordinates
endPoint cylinder =
    Point3d.along (axis cylinder) (Quantity.multiplyBy 0.5 (length cylinder))


{-| Get the circle at the start of a cylinder. The axial direction of this
circle will be the _reverse_ of the axial direction of the cylinder itself (the
circle axial direction will point backwards/outwards).
-}
startCap : Cylinder3d units coordinates -> Circle3d units coordinates
startCap cylinder =
    Circle3d.withRadius (radius cylinder)
        (Direction3d.reverse (axialDirection cylinder))
        (startPoint cylinder)


{-| Get the circle at the end of a cylinder. The axial direction of this circle
will be the same as that of the cylinder itself.
-}
endCap : Cylinder3d units coordinates -> Circle3d units coordinates
endCap cylinder =
    Circle3d.withRadius (radius cylinder)
        (axialDirection cylinder)
        (endPoint cylinder)


{-| Get the volume of a cylinder.
-}
volume : Cylinder3d units coordinates -> Quantity Float (Cubed units)
volume cylinder =
    Quantity.multiplyBy pi (Quantity.squared (radius cylinder))
        |> Quantity.times (length cylinder)


{-| Get the minimal bounding box containing a given cylinder.
-}
boundingBox : Cylinder3d units coordinates -> BoundingBox3d units coordinates
boundingBox cylinder =
    BoundingBox3d.union
        (Circle3d.boundingBox (startCap cylinder))
        (Circle3d.boundingBox (endCap cylinder))


{-| Check if a cylinder contains a given point.
-}
contains : Point3d units coordinates -> Cylinder3d units coordinates -> Bool
contains point cylinder =
    let
        cylinderAxis =
            axis cylinder

        axialDistance =
            Quantity.abs (Point3d.signedDistanceAlong cylinderAxis point)

        radialDistance =
            Point3d.distanceFromAxis cylinderAxis point
    in
    (axialDistance |> Quantity.lessThanOrEqualTo (Quantity.half (length cylinder)))
        && (radialDistance |> Quantity.lessThanOrEqualTo (radius cylinder))


{-| Reverse a cylinder so that the start point becomes the end point and vice
versa. This also means that the axial direction will be reversed.
-}
reverse : Cylinder3d units coordinates -> Cylinder3d units coordinates
reverse (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.reverse cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Scale a cylinder about a given point by a given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Cylinder3d units coordinates -> Cylinder3d units coordinates
scaleAbout point scale cylinder =
    let
        scaledCenterPoint =
            Point3d.scaleAbout point scale (centerPoint cylinder)

        scaledDirection =
            if scale >= 0 then
                axialDirection cylinder

            else
                Direction3d.reverse (axialDirection cylinder)

        scaledRadius =
            Quantity.abs (Quantity.multiplyBy scale (radius cylinder))

        scaledLength =
            Quantity.abs (Quantity.multiplyBy scale (length cylinder))
    in
    Types.Cylinder3d
        { axis = Axis3d.through scaledCenterPoint scaledDirection
        , radius = scaledRadius
        , length = scaledLength
        }


{-| Rotate a cylinder around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Cylinder3d units coordinates -> Cylinder3d units coordinates
rotateAround givenAxis givenAngle (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.rotateAround givenAxis givenAngle cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Translate a cylinder by a given displacement.
-}
translateBy : Vector3d units coordinates -> Cylinder3d units coordinates -> Cylinder3d units coordinates
translateBy displacement (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.translateBy displacement cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Translate a cylinder in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Cylinder3d units coordinates -> Cylinder3d units coordinates
translateIn givenDirection givenDistance (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.translateIn givenDirection givenDistance cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Mirror a cylinder across a given plane.
-}
mirrorAcross : Plane3d units coordinates -> Cylinder3d units coordinates -> Cylinder3d units coordinates
mirrorAcross plane (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.mirrorAcross plane cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Convert a cylinder from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Cylinder3d units1 coordinates -> Cylinder3d units2 coordinates
at rate (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.at rate cylinder.axis
        , radius = Quantity.abs (Quantity.at rate cylinder.radius)
        , length = Quantity.abs (Quantity.at rate cylinder.length)
        }


{-| Convert a cylinder from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Cylinder3d units1 coordinates -> Cylinder3d units2 coordinates
at_ rate (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.at_ rate cylinder.axis
        , radius = Quantity.abs (Quantity.at_ rate cylinder.radius)
        , length = Quantity.abs (Quantity.at_ rate cylinder.length)
        }


{-| Take a cylinder considered to be defined in local coordinates relative to a
given reference frame, and return that cylinder expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Cylinder3d units localCoordinates -> Cylinder3d units globalCoordinates
placeIn frame (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.placeIn frame cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }


{-| Take a cylinder defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Cylinder3d units globalCoordinates -> Cylinder3d units localCoordinates
relativeTo frame (Types.Cylinder3d cylinder) =
    Types.Cylinder3d
        { axis = Axis3d.relativeTo frame cylinder.axis
        , radius = cylinder.radius
        , length = cylinder.length
        }
