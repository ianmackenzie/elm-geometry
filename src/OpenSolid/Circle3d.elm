module OpenSolid.Circle3d
    exposing
        ( centerPoint
        , axialDirection
        , radius
        , diameter
        , area
        , circumference
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle3d.svg" alt="Circle3d" width="160">

A `Circle3d` is defined by its center point, axial direction and radius. The
axial direction is the direction of the axis through the center of the circle
that all points on the circle are equidistant from, or equivalently the normal
direction of the plane defined by the circle. Currently you can only do a few
basic things with circles, such as measuring the area or circumference, but this
should increase in the future.

Circles can be constructed by passing a record with `centerPoint`,
`axialDirection` and `radius` fields to the `Circle3d` constructor, for example

    exampleCircle =
        Circle3d
            { centerPoint = Point3d ( 2, 0, 1 )
            , axialDirection = Direction3d.z
            , radius = 3
            }

**You must ensure the provided radius is positive** (or zero, but that's not a
very useful circle).


# Accessors

@docs centerPoint, axialDirection, radius, diameter, area, circumference


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction3d as Direction3d


{-| Get the center point of a circle.

    Circle3d.centerPoint exampleCircle
    --> Point3d ( 2, 0, 1 )

-}
centerPoint : Circle3d -> Point3d
centerPoint (Circle3d properties) =
    properties.centerPoint


{-| Get the axial direction of a circle.

    Circle3d.axialDirection exampleCircle
    --> Direction3d.z

-}
axialDirection : Circle3d -> Direction3d
axialDirection (Circle3d properties) =
    properties.axialDirection


{-| Get the radius of a circle.

    Circle3d.radius exampleCircle
    --> 3

-}
radius : Circle3d -> Float
radius (Circle3d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circl3d.diameter exampleCircle
    --> 6

-}
diameter : Circle3d -> Float
diameter circle =
    2 * radius circle


{-| Get the area of a circle.

    Circle3d.area exampleCircle
    --> 28.2743

-}
area : Circle3d -> Float
area circle =
    let
        r =
            radius circle
    in
        pi * r * r


{-| Get the circumference of a circle.

    Circle3d.circumference exampleCircle
    --> 18.8496

-}
circumference : Circle3d -> Float
circumference circle =
    2 * pi * radius circle


{-| Scale a circle around a given point by a given scale.

    Circle3d.scaleAbout Point3d.origin 3 exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 6, 0, 3 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 9
    -->     }

-}
scaleAbout : Point3d -> Float -> Circle3d -> Circle3d
scaleAbout point scale circle =
    Circle3d
        { centerPoint = Point3d.scaleAbout point scale (centerPoint circle)
        , radius = abs (scale * radius circle)
        , axialDirection =
            if scale >= 0 then
                axialDirection circle
            else
                Direction3d.flip (axialDirection circle)
        }


{-| Rotate a circle around a given axis by a given angle (in radians).

    Circle3d.rotateAround Axis3d.y (degrees 90) exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 1, 0, -2 )
    -->     , axialDirection = Direction3d.x
    -->     , radius = 3
    -->     }

-}
rotateAround : Axis3d -> Float -> Circle3d -> Circle3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \circle ->
            Circle3d
                { centerPoint = rotatePoint (centerPoint circle)
                , radius = radius circle
                , axialDirection = rotateDirection (axialDirection circle)
                }


{-| Translate a circle by a given displacement.

    displacement =
        Vector3d ( 2, 1, 3 )

    Circle3d.translateBy displacement exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 4, 1, 4 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
translateBy : Vector3d -> Circle3d -> Circle3d
translateBy displacement circle =
    Circle3d
        { centerPoint = Point3d.translateBy displacement (centerPoint circle)
        , radius = radius circle
        , axialDirection = axialDirection circle
        }


{-| Mirror a circle across a given plane.

    Circle3d.mirrorAcross Plane3d.xy exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 2, 0, -1 )
    -->     , axialDirection = Direction3d ( 0, 0, -1 )
    -->     , radius = 3
    -->     }

-}
mirrorAcross : Plane3d -> Circle3d -> Circle3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \circle ->
            Circle3d
                { centerPoint = mirrorPoint (centerPoint circle)
                , radius = radius circle
                , axialDirection = mirrorDirection (axialDirection circle)
                }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Circle3d.relativeTo localFrame exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 1, -2, -2 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
relativeTo : Frame3d -> Circle3d -> Circle3d
relativeTo frame circle =
    Circle3d
        { centerPoint = Point3d.relativeTo frame (centerPoint circle)
        , radius = radius circle
        , axialDirection = Direction3d.relativeTo frame (axialDirection circle)
        }


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Circle3d.placeIn localFrame exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 3, 2, 4 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
placeIn : Frame3d -> Circle3d -> Circle3d
placeIn frame circle =
    Circle3d
        { centerPoint = Point3d.placeIn frame (centerPoint circle)
        , radius = radius circle
        , axialDirection = Direction3d.placeIn frame (axialDirection circle)
        }


{-| Get the minimal bounding box containing a given circle.

    Circle3d.boundingBox exampleCircle
    --> BoundingBox3d
    -->     { minX = -1
    -->     , maxX = 5
    -->     , minY = -3
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 1
    -->     }

-}
boundingBox : Circle3d -> BoundingBox3d
boundingBox circle =
    let
        ( nx, ny, nz ) =
            Direction3d.components (axialDirection circle)

        nx2 =
            nx * nx

        ny2 =
            ny * ny

        nz2 =
            nz * nz

        r =
            radius circle

        dx =
            r * sqrt (ny2 + nz2)

        dy =
            r * sqrt (nx2 + nz2)

        dz =
            r * sqrt (nx2 + ny2)

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint circle)
    in
        BoundingBox3d
            { minX = cx - dx
            , maxX = cx + dx
            , minY = cy - dy
            , maxY = cy + dy
            , minZ = cz - dz
            , maxZ = cz + dz
            }
