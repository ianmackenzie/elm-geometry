{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Plane3d
    exposing
        ( xy
        , yz
        , zx
        , originPoint
        , normalDirection
        , sketchPlane
        , offsetBy
        , flip
        , normalAxis
        , rotateAround
        , translateBy
        , moveTo
        , mirrorAcross
        , relativeTo
        , placeIn
        , encode
        , decoder
        )

{-| Various functions for creating and working with `Plane3d` values. For the
examples below, assume that all OpenSolid core types have been imported using

    import OpenSolid.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Plane3d as Plane3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Predefined planes

@docs xy, yz, zx

# Constructors

Planes can by constructed by passing a record with `originPoint` and
`normalDirection` fields to the `Plane3d` constructor, for example:

    plane =
        Plane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , normalDirection = Direction3d.y
            }

Future OpenSolid versions will likely add specialized constructors such as for
constructing a plane through three points, the midplane between two points, or
the midplane between two other planes.

# Accessors

@docs originPoint, normalDirection

# Conversions

@docs normalAxis, sketchPlane

# Transformations

@docs offsetBy, flip, rotateAround, translateBy, moveTo, mirrorAcross

# Coordinate frames

@docs relativeTo, placeIn

# JSON serialization

@docs encode, decoder
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d


{-| The global XY plane, centered at the origin with a normal in the positive Z
direction.

    Plane3d.xy ==
        Plane3d
            { originPoint = Point3d.origin
            , normalDirection = Direction3d.z
            }
-}
xy : Plane3d
xy =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.z
        }


{-| The global YZ plane, centered at the origin with a normal in the positive X
direction.

    Plane3d.yz ==
        Plane3d
            { originPoint = Point3d.origin
            , normalDirection = Direction3d.x
            }
-}
yz : Plane3d
yz =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.x
        }


{-| The global ZX plane, centered at the origin with a normal in the positive Y
direction.

    Plane3d.zx ==
        Plane3d
            { originPoint = Point3d.origin
            , normalDirection = Direction3d.y
            }
-}
zx : Plane3d
zx =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.y
        }


{-| Get the origin point of a plane.

    Plane3d.originPoint Plane3d.xy ==
        Point3d.origin
-}
originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


{-| Get the normal direction of a plane.

    Plane3d.normalDirection Plane3d.xy ==
        Direction3d.z
-}
normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection


{-| Construct an axis from the origin point and normal direction of a plane.

    Plane3d.normalAxis Plane3d.zx ==
        Axis3d.y
-}
normalAxis : Plane3d -> Axis3d
normalAxis plane =
    Axis3d
        { originPoint = originPoint plane
        , direction = normalDirection plane
        }


{-| Construct a SketchPlane3d from the given plane. The origin of the sketch
plane will be the origin point of the given plane. The X and Y basis directions
of the sketch plane

  - will be perpendicular to each other
  - will both be perpendicular to the normal direction of the given plane
  - will have a cross product equal to the normal direction of the given plane

but are otherwise arbitrary. For example, in the current implementation,

    Plane3d.sketchPlane Plane3d.xy ==
        SketchPlane3d
            { originPoint = Point3d.origin
            , xDirection = Direction3d ( 0, -1, 0 )
            , yDirection = Direction3d ( 1, 0, 0 )
            }

which is coplanar with and has the same origin point as`SketchPlane3d.xy`, but
is not equal to it as might be expected.

As a result, this function is only useful if the exact X and Y basis directions
of the resulting sketch plane are not important; if they are, you will need to
construct those directions explicitly and directly construct a new
`SketchPlane3d` from them.
-}
sketchPlane : Plane3d -> SketchPlane3d
sketchPlane plane =
    let
        normal =
            normalDirection plane

        xDirection =
            Direction3d.perpendicularTo normal

        yDirectionVector =
            Direction3d.crossProduct normal xDirection

        yDirection =
            Direction3d (Vector3d.components yDirectionVector)
    in
        SketchPlane3d
            { originPoint = originPoint plane
            , xDirection = xDirection
            , yDirection = yDirection
            }


{-| Shift a plane in its own normal direction by the given (signed) distance.

    Plane3d.offsetBy 1.0 Plane3d.zx ==
        Plane3d
            { originPoint = Point3d ( 0, 1, 0 )
            , normalDirection = Direction3d.y
            }

    Plane3d.offsetBy -2.0 Plane3d.xy ==
        Plane3d
            { originPoint = Point3d ( 0, 0, -2 )
            , normalDirection = Direction3d.z
            }
-}
offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    translateBy (Direction3d.times distance (normalDirection plane)) plane


{-| Reverse a plane's normal direction while leaving its origin point unchanged.

    Plane3d.flip Plane3d.xy ==
        Plane3d
            { originPoint = Point3d.origin
            , normalDirection = Direction3d ( 0, 0, -1 )
            }
-}
flip : Plane3d -> Plane3d
flip plane =
    Plane3d
        { originPoint = originPoint plane
        , normalDirection = Direction3d.negate (normalDirection plane)
        }


{-| Rotate a plane around an axis by a given angle.

    Plane3d.rotateAround Axis3d.y (degrees 90) Plane3d.xy ==
        Plane3d.yz
-}
rotateAround : Axis3d -> Float -> Plane3d -> Plane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \plane ->
            Plane3d
                { originPoint = rotatePoint (originPoint plane)
                , normalDirection = rotateDirection (normalDirection plane)
                }


{-| Translate a plane by a given displacement. Applies the given displacement to
the plane's origin point and leaves its normal direction unchanged.

    plane =
        Plane3d
            { originPoint = Point3d ( 1, 1, 1 )
            , normalDirection = Direction3d.z
            }

    displacement =
        Vector3d ( 1, 2, 3 )

    Plane3d.translateBy displacement plane ==
        Plane3d
            { originPoint = Point3d ( 2, 3, 4 )
            , normalDirection = Direction3d.z
            }
-}
translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector plane =
    Plane3d
        { originPoint = Point3d.translateBy vector (originPoint plane)
        , normalDirection = normalDirection plane
        }


{-| Move a plane so that it has the given origin point but unchanged normal
direction.

    newOrigin =
        Point3d ( 1, 2, 3 )

    Plane3d.moveTo newOrigin Plane3d.xy ==
        Plane3d
            { originPoint = Point3d ( 1, 2, 3 )
            , normalDirection = Direction3d.z
            }
-}
moveTo : Point3d -> Plane3d -> Plane3d
moveTo newOrigin plane =
    Plane3d
        { originPoint = newOrigin
        , normalDirection = normalDirection plane
        }


{-| Mirror one plane across another. The plane to mirror across is given first
and the plane to mirror is given second.

    plane =
        Plane3d
            { originPoint = Point3d ( 1, 2, 3 )
            , normalDirection = Direction3d.z
            }

    Plane3d.mirrorAcross Plane3d.xy plane ==
        Plane3d
            { originPoint = Point3d ( 1, 2, -3 )
            , normalDirection = Direction3d ( 0, 0, -1 )
            }
-}
mirrorAcross : Plane3d -> Plane3d -> Plane3d
mirrorAcross otherPlane =
    let
        mirrorPoint =
            Point3d.mirrorAcross otherPlane

        mirrorDirection =
            Direction3d.mirrorAcross otherPlane
    in
        \plane ->
            Plane3d
                { originPoint = mirrorPoint (originPoint plane)
                , normalDirection = mirrorDirection (normalDirection plane)
                }


{-| Take an axis currently expressed in global coordinates and express it
relative to a given frame. For example, consider a frame raised up one unit
above the global XYZ frame and rotated 45 degrees clockwise around the Z axis:

    newOrigin =
        Point3d ( 0, 0, 1 )

    frame =
        Frame3d.xyz
            |> Frame3d.moveTo newOrigin
            |> Frame3d.rotateAround Axis3d.z (degrees -45)

Relative to this frame, the global YZ plane has an origin point one unit below
the origin, with a normal direction halfway between the X and Y directions:

    Plane3d.relativeTo frame Plane3d.yz ==
        Plane3d
            { originPoint = Point3d ( 0, 0, -1 )
            , normalDirection = Direction3d ( 0.7071, 0.7071, 0 )
            }
-}
relativeTo : Frame3d -> Plane3d -> Plane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \plane ->
            Plane3d
                { originPoint = relativePoint (originPoint plane)
                , normalDirection = relativeDirection (normalDirection plane)
                }


{-| Place a plane in a given frame, considering it as being expressed relative
to that frame and returning the corresponding plane in global coordinates.
Inverse of `relativeTo`.

For example, consider a frame raised up one unit above the global XYZ frame and
rotated 45 degrees clockwise around the Z axis:

    newOrigin =
        Point3d ( 0, 0, 1 )

    frame =
        Frame3d.xyz
            |> Frame3d.moveTo newOrigin
            |> Frame3d.rotateAround Axis3d.z (degrees -45)

Now, consider a plane through the point (0, 0, 1) with a normal in the positive
X direction (parallel to the YZ plane):

    plane =
        Plane3d
            { originPoint = Point3d ( 0, 0, 1 )
            , normalDirection = Direction3d.x
            }

Placing this plane in the given frame gives a plane with an origin point two
units above the global origin point, with the X direction of the rotated frame:

    Plane3d.placeIn frame plane ==
        Plane3d
            { originPoint = Point3d ( 0, 0, 2 )
            , normalDirection = Direction3d ( 0.7071, -0.7071, 0 )
            }

-}
placeIn : Frame3d -> Plane3d -> Plane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \plane ->
            Plane3d
                { originPoint = placePoint (originPoint plane)
                , normalDirection = placeDirection (normalDirection plane)
                }


{-| Encode an Plane3d as a JSON object with 'originPoint' and 'normalDirection'
fields.
-}
encode : Plane3d -> Value
encode plane =
    Encode.object
        [ ( "originPoint", Point3d.encode (originPoint plane) )
        , ( "normalDirection", Direction3d.encode (normalDirection plane) )
        ]


{-| Decoder for Plane3d values from JSON objects with 'originPoint' and
'normalDirection' fields.
-}
decoder : Decoder Plane3d
decoder =
    Decode.object2
        (\originPoint normalDirection ->
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
        )
        ("originPoint" := Point3d.decoder)
        ("normalDirection" := Direction3d.decoder)
