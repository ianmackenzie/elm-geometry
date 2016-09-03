{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Axis2d
    exposing
        ( x
        , y
        , perpendicularTo
        , originPoint
        , direction
        , flip
        , rotateAround
        , translateBy
        , moveTo
        , mirrorAcross
        , relativeTo
        , placeIn
        , encode
        , decoder
        )

{-| Various functions for creating and working with `Axis2d` values. For the
examples below, assume that all OpenSolid core types have been imported using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Axis2d as Axis2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Predefined axes

@docs x, y

# Constructors

Axes can by constructed by passing a record with `originPoint` and `direction`
fields to the `Axis2d` constructor, for example:

    axis =
        Axis2d
            { originPoint = Point2d ( 2, 3 )
            , direction = Direction2d.fromAngle (degrees 45)
            }

Future versions of OpenSolid may add specialized constructors such as for
constructing an axis through two points.

@docs perpendicularTo

# Accessors

@docs originPoint, direction

# Transformations

@docs flip, rotateAround, translateBy, moveTo, mirrorAcross

# Coordinate frames

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

# JSON serialization

@docs encode, decoder
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d


{-| The global X axis.

    Axis2d.x ==
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.x
            }
-}
x : Axis2d
x =
    Axis2d { originPoint = Point2d.origin, direction = Direction2d.x }


{-| The global Y axis.

    Axis2d.y ==
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.y
            }
-}
y : Axis2d
y =
    Axis2d { originPoint = Point2d.origin, direction = Direction2d.y }


{-| Construct an axis perpendicular to another axis by rotating the given axis
90 degrees counterclockwise around its own origin point.

    Axis2d.perpendicularTo Axis2d.x ==
        Axis2d.y

    Axis2d.perpendicularTo Axis2d.y ==
        Axis2d.flip Axis2d.x
-}
perpendicularTo : Axis2d -> Axis2d
perpendicularTo axis =
    Axis2d
        { originPoint = originPoint axis
        , direction = Direction2d.perpendicularTo (direction axis)
        }


{-| Get the origin point of an axis.

    Axis2d.originPoint Axis2d.x ==
        Point2d.origin
-}
originPoint : Axis2d -> Point2d
originPoint (Axis2d properties) =
    properties.originPoint


{-| Get the direction of an axis.

    Axis2d.direction Axis2d.y ==
        Direction2d.y
-}
direction : Axis2d -> Direction2d
direction (Axis2d properties) =
    properties.direction


{-| Reverse the direction of an axis while keeping the same origin point.

    Axis2d.flip Axis2d.x ==
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d ( -1, 0 )
            }
-}
flip : Axis2d -> Axis2d
flip axis =
    Axis2d
        { originPoint = originPoint axis
        , direction = Direction2d.negate (direction axis)
        }


{-| Rotate an axis around a given center point by a given angle. Rotates the
axis' origin point around the given point by the given angle and the axis'
direction by the given angle.

    Axis2d.rotateAround Point2d.origin (degrees 90) Axis2d.x ==
        Axis2d.y
-}
rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \axis ->
            Axis2d
                { originPoint = rotatePoint (originPoint axis)
                , direction = rotateDirection (direction axis)
                }


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.

    displacement =
        Vector2d ( 2, 3 )

    Axis2d.translateBy displacement Axis2d.y ==
        Axis2d
            { originPoint = Point2d ( 2, 3 )
            , direction = Direction2d.y
            }
-}
translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector axis =
    Axis2d
        { originPoint = Point2d.translateBy vector (originPoint axis)
        , direction = direction axis
        }


{-| Move an axis so that it has the given origin point but unchanged direction.

    axis =
        Axis2d
            { originPoint = Point2d ( 2, 3 )
            , direction = Direction2d.y
            }

    newOrigin =
        Point2d ( 4, 5 )

    Axis2d.moveTo newOrigin axis ==
        Axis2d
            { originPoint = Point2d ( 4, 5 ),
            , direction = Direction2d.y
            }
-}
moveTo : Point2d -> Axis2d -> Axis2d
moveTo newOrigin axis =
    Axis2d { originPoint = newOrigin, direction = direction axis }


{-| Mirror one axis across another. The axis to mirror across is given first and
the axis to mirror is given second.

    axis =
        Axis2d
            { originPoint = Point2d ( 1, 2 )
            , direction = Direction2d.fromAngle (degrees 30)
            }

    mirrorAxis =
        Axis2d.x

    Axis2d.mirrorAcross mirrorAxis axis ==
        Axis2d
            { originPoint = Point2d ( 1, -2 )
            , direction = Direction2d.fromAngle (degrees -30)
            }
-}
mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis =
    let
        mirrorPoint =
            Point2d.mirrorAcross otherAxis

        mirrorDirection =
            Direction2d.mirrorAcross otherAxis
    in
        \axis ->
            Axis2d
                { originPoint = mirrorPoint (originPoint axis)
                , direction = mirrorDirection (direction axis)
                }


{-| Take an axis currently expressed in global coordinates and express it
relative to a given frame. For example, an axis at a 45 degree angle, expressed
relative to a frame inclined at a 30 degree angle, is an axis at only a 15
degree angle:

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

    axis =
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.fromAngle (degrees 45)
            }

    Axis2d.relativeTo rotatedFrame axis ==
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.fromAngle (degrees 15)
            }
-}
relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame =
    let
        relativePoint =
            Point2d.relativeTo frame

        relativeDirection =
            Direction2d.relativeTo frame
    in
        \axis ->
            Axis2d
                { originPoint = relativePoint (originPoint axis)
                , direction = relativeDirection (direction axis)
                }


{-| Place an axis in a given frame, considering it as being expressed relative
to that frame and returning the corresponding axis in global coordinates.
Inverse of `relativeTo`. For example, an axis at a 15 degree angle, placed in a
frame already inclined at a 30 degree angle, is an axis at a 45 degree angle:

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

    axis =
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.fromAngle (degrees 15)
            }

    Axis2d.placeIn rotatedFrame axis ==
        Axis2d
            { originPoint = Point2d.origin
            , direction = Direction2d.fromAngle (degrees 45)
            }
-}
placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame =
    let
        placePoint =
            Point2d.placeIn frame

        placeDirection =
            Direction2d.placeIn frame
    in
        \axis ->
            Axis2d
                { originPoint = placePoint (originPoint axis)
                , direction = placeDirection (direction axis)
                }


{-| Encode an Axis2d as a JSON object with 'originPoint' and 'direction' fields.
-}
encode : Axis2d -> Value
encode axis =
    Encode.object
        [ ( "originPoint", Point2d.encode (originPoint axis) )
        , ( "direction", Direction2d.encode (direction axis) )
        ]


{-| Decoder for Axis2d values from JSON objects with 'originPoint' and
'direction' fields.
-}
decoder : Decoder Axis2d
decoder =
    Decode.object2
        (\originPoint direction ->
            Axis2d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := Point2d.decoder)
        ("direction" := Direction2d.decoder)
