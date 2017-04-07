--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.Plane3d
    exposing
        ( xy
        , yz
        , zx
        , throughPoints
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
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/plane3d.svg" alt="Plane3d" width="160">

A `Plane3d` is an infinite flat plane in 3D. It is defined by an origin point
and normal direction and is useful for several operations including:

  - Mirroring across the plane
  - Projecting onto the plane
  - Measuring distance from the plane

Planes can by constructed by passing a record with `originPoint` and
`normalDirection` fields to the `Plane3d` constructor, for example:

    plane =
        Plane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , normalDirection = Direction3d.y
            }


# Predefined planes

@docs xy, yz, zx


# Constructors

@docs throughPoints


# Accessors

@docs originPoint, normalDirection


# Conversions

@docs normalAxis, sketchPlane


# Transformations

@docs offsetBy, flip, rotateAround, translateBy, moveTo, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Triangle3d as Triangle3d


{-| The global XY plane, centered at the origin with a normal in the positive Z
direction.

    Plane3d.xy
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d.z
    -->     }

-}
xy : Plane3d
xy =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.z
        }


{-| The global YZ plane, centered at the origin with a normal in the positive X
direction.

    Plane3d.yz
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d.x
    -->     }

-}
yz : Plane3d
yz =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.x
        }


{-| The global ZX plane, centered at the origin with a normal in the positive Y
direction.

    Plane3d.zx
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d.y
    -->     }

-}
zx : Plane3d
zx =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.y
        }


{-| Attempt to construct a plane passing through the three given points. The
origin point of the resulting plane will be equal to the first given point, and
the normal direction will be such that the three given points are in
counterclockwise order around it according to the right-hand rule. If the three
given points are collinear, returns `Nothing`.

    Plane3d.throughPoints
        (Point3d ( 2, 0, 0 ))
        (Point3d ( 3, 0, 0 ))
        (Point3d ( 4, 1, 1 ))
    --> Just
    -->     (Plane3d
    -->         { originPoint = Point3d ( 2, 0, 0 )
    -->         , normalDirection = Direction3d ( 0, -0.7071, 0.7071 )
    -->         }
    -->     )

    Plane3d.throughPoints
        (Point3d ( 2, 0, 0 ))
        (Point3d ( 3, 0, 0 ))
        (Point3d ( 4, 0, 0 ))
    --> Nothing

-}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe Plane3d
throughPoints firstPoint secondPoint thirdPoint =
    Triangle3d ( firstPoint, secondPoint, thirdPoint )
        |> Triangle3d.normalDirection
        |> Maybe.map
            (\normalDirection ->
                Plane3d
                    { originPoint = firstPoint
                    , normalDirection = normalDirection
                    }
            )


{-| Get the origin point of a plane.

    Plane3d.originPoint Plane3d.xy
    --> Point3d.origin

-}
originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


{-| Get the normal direction of a plane.

    Plane3d.normalDirection Plane3d.xy
    --> Direction3d.z

-}
normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection


{-| Construct an axis from the origin point and normal direction of a plane.

    Plane3d.normalAxis Plane3d.zx
    --> Axis3d.y

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

  - will be perpendicular to each other,
  - will both be perpendicular to the normal direction of the given plane, and
  - will have a cross product equal to the normal direction of the given plane

but are otherwise arbitrary. For example, in the current implementation,

    Plane3d.sketchPlane Plane3d.xy
    --> SketchPlane3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d ( 0, -1, 0 )
    -->     , yDirection = Direction3d ( 1, 0, 0 )
    -->     }

which is coplanar with and has the same origin point as [`SketchPlane3d.xy`](OpenSolid-SketchPlane3d#xy),
but is not equal to it as might be expected.

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

        ( xDirection, yDirection ) =
            Direction3d.perpendicularBasis normal
    in
        SketchPlane3d
            { originPoint = originPoint plane
            , xDirection = xDirection
            , yDirection = yDirection
            }


{-| Shift a plane in its own normal direction by the given (signed) distance.

    Plane3d.offsetBy 1.0 Plane3d.zx
    --> Plane3d
    -->     { originPoint = Point3d ( 0, 1, 0 )
    -->     , normalDirection = Direction3d.y
    -->     }

    Plane3d.offsetBy -2.0 Plane3d.xy
    --> Plane3d
    -->     { originPoint = Point3d ( 0, 0, -2 )
    -->     , normalDirection = Direction3d.z
    -->     }

-}
offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    translateBy (Vector3d.in_ (normalDirection plane) distance) plane


{-| Reverse a plane's normal direction while leaving its origin point unchanged.

    Plane3d.flip Plane3d.xy
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d ( 0, 0, -1 )
    -->     }

-}
flip : Plane3d -> Plane3d
flip plane =
    Plane3d
        { originPoint = originPoint plane
        , normalDirection = Direction3d.flip (normalDirection plane)
        }


{-| Rotate a plane around an axis by a given angle.

    Plane3d.rotateAround Axis3d.y (degrees 90) Plane3d.xy
    --> Plane3d.yz

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

    Plane3d.translateBy displacement plane
    --> Plane3d
    -->     { originPoint = Point3d ( 2, 3, 4 )
    -->     , normalDirection = Direction3d.z
    -->     }

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

    Plane3d.moveTo newOrigin Plane3d.xy
    --> Plane3d
    -->     { originPoint = Point3d ( 1, 2, 3 )
    -->     , normalDirection = Direction3d.z
    -->     }

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

    Plane3d.mirrorAcross Plane3d.xy plane
    --> Plane3d
    -->     { originPoint = Point3d ( 1, 2, -3 )
    -->     , normalDirection = Direction3d ( 0, 0, -1 )
    -->     }

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


{-| Take a plane defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    referenceFrame =
        Frame3d.at (Point3d ( 1, 1, 1 ))

    plane =
        Plane3d
            { originPoint = Point3d ( 0, 0, 2 )
            , normalDirection = Direction3d.z
            }

    Plane3d.relativeTo referenceFrame plane
    --> Plane3d
    -->     { originPoint = Point3d ( -1, -1, 1 )
    -->     , normalDirection = Direction3d.z
    -->     }

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


{-| Take a plane defined in local coordinates relative to a given reference
frame, and return that plane expressed in global coordinates.

    referenceFrame =
        Frame3d.at (Point3d ( 1, 1, 1 ))

    plane =
        Plane3d
            { originPoint = Point3d ( 1, 2, 3 )
            , normalDirection = Direction3d.z
            }

    Plane3d.placeIn referenceFrame plane
    --> Plane3d
    -->     { originPoint = Point3d ( 2, 3, 4 )
    -->     , normalDirection = Direction3d.z
    -->     }

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
