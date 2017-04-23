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


module OpenSolid.SketchPlane3d
    exposing
        ( xy
        , yx
        , yz
        , zy
        , zx
        , xz
        , throughPoints
        , originPoint
        , xDirection
        , yDirection
        , normalDirection
        , xAxis
        , yAxis
        , normalAxis
        , plane
        , flipX
        , flipY
        , moveTo
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/sketchPlane3d.svg" alt="SketchPlane3d" width="160">

A `SketchPlane3d` represents a 2D planar coordinate system in 3D space, and is
defined by its origin point and X and Y directions (which are always
perpendicular to each other). Sketch planes are the primary tool for converting
back and forth between 2D and 3D coordinates:

  - 3D geometry such as points, directions and line segments can be projected
    *into* a sketch plane, which effectively projects the geometry *onto* the
    sketch plane and then expresses the projected geometry *in* 2D coordinates.
  - 2D geometry can be place *onto* a sketch plane to result in 3D geometry. For
    example, a 2D point placed onto a sketch plane will result in a 3D point
    *on* that sketch plane that has the given 2D coordinate *in* the sketch
    plane.

Many 3D data types have `projectInto` functions that return the corresponding 2D
data type, and those 2D data types have `placeOnto` functions for converting
back to 3D. For example, [`Triangle3d.projectInto`](OpenSolid-Triangle3d#projectInto)
returns a `Triangle2d` and [`Triangle2d.placeOnto`](OpenSolid-Triangle2d#placeOnto)
returns a `Triangle3d`. These pairs of functions are almost, but not quite,
inverses of each other:

  - <code>Point2d.placeOnto&nbsp;sketchPlane&nbsp;>>&nbsp;Point3d.projectInto&nbsp;sketchPlane</code>
    will just return the original `Point2d` (within roundoff error).
  - <code>Point3d.projectInto&nbsp;sketchPlane&nbsp;>>&nbsp;Point2d.placeOnto&nbsp;sketchPlane</code>
    is equivalent to <code>Point3d.projectOnto&nbsp;(SketchPlane3d.plane&nbsp;sketchPlane)</code>
    since the result will always be on the given sketch plane.

Sketch planes can be constructed explicitly by passing a record with
`originPoint`, `xDirection` and `yDirection` fields to the `SketchPlane3d`
constructor, for example

    sketchPlane =
        SketchPlane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d.positiveY
            , yDirection = Direction3d.negativeZ
            }

If you construct a `SketchPlane3d` this way, **you must ensure that the X and Y
basis directions are perpendicular to each other**.


# Predefined sketch planes

These predefined sketch planes all have the global origin point as their origin
point, and use the two indicated global axes as their X and Y axes. For example,

    SketchPlane3d.yz
    --> SketchPlane3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.y
    -->     , yDirection = Direction3d.z
    -->     }

@docs xy, yx, yz, zy, zx, xz


# Constructors

@docs throughPoints


# Accessors

@docs originPoint, xDirection, yDirection, normalDirection


# Axes

@docs xAxis, yAxis, normalAxis


# Conversions

@docs plane


# Transformations

@docs flipX, flipY, moveTo, rotateAround, rotateAroundOwn, translateBy, translateAlongOwn, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Axis3d as Axis3d


{-| A sketch plane formed from the global X and Y axes.
-}
xy : SketchPlane3d
xy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Y and X axes.
-}
yx : SketchPlane3d
yx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global Y and Z axes.
-}
yz : SketchPlane3d
yz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


{-| A sketch plane formed from the global Z and Y axes.
-}
zy : SketchPlane3d
zy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Z and X axes.
-}
zx : SketchPlane3d
zx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global X and Z axes.
-}
xz : SketchPlane3d
xz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


{-| Attempt to construct a sketch plane that passes through the three given
points. Returns a sketch plane where:

  - The origin point is the first given point
  - The X direction is equal to the direction from the first given point to the
    second
  - The Y direction is chosen such that the third given point lies on the sketch
    plane and has a positive Y coordinate within the sketch plane (that is, it
    is on the positive Y side of the sketch plane's X axis)

If the three given points are collinear, returns `Nothing`.

    SketchPlane3d.throughPoints
        (Point3d ( 2, 0, 0 ))
        (Point3d ( 3, 0, 0 ))
        (Point3d ( 4, 1, 1 ))
    --> Just
    -->     (SketchPlane3d
    -->         { originPoint = Point3d ( 2, 0, 0 )
    -->         , xDirection = Direction3d.x
    -->         , yDirection = Direction3d ( 0, 0.7071, 0.7071 )
    -->         }
    -->     )

    SketchPlane3d.throughPoints
        (Point3d ( 2, 0, 0 ))
        (Point3d ( 3, 0, 0 ))
        (Point3d ( 4, 0, 0 ))
    --> Nothing

-}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe SketchPlane3d
throughPoints firstPoint secondPoint thirdPoint =
    Vector3d.direction (Point3d.vectorFrom firstPoint secondPoint)
        |> Maybe.andThen
            (\xDirection ->
                let
                    firstCandidate =
                        Point3d.vectorFrom firstPoint thirdPoint

                    secondCandidate =
                        Point3d.vectorFrom secondPoint thirdPoint

                    firstSquaredLength =
                        Vector3d.squaredLength firstCandidate

                    secondSquaredLength =
                        Vector3d.squaredLength secondCandidate

                    chosenVector =
                        if firstSquaredLength <= secondSquaredLength then
                            firstCandidate
                        else
                            secondCandidate

                    xDirectionVector =
                        Direction3d.toVector xDirection

                    normalVector =
                        Vector3d.crossProduct xDirectionVector chosenVector

                    yVector =
                        Vector3d.crossProduct normalVector xDirectionVector
                in
                    Vector3d.direction yVector
                        |> Maybe.map
                            (\yDirection ->
                                SketchPlane3d
                                    { originPoint = firstPoint
                                    , xDirection = xDirection
                                    , yDirection = yDirection
                                    }
                            )
            )


{-| Get the origin point of a sketch plane.

    SketchPlane3d.originPoint SketchPlane3d.xy
    --> Point3d.origin

-}
originPoint : SketchPlane3d -> Point3d
originPoint (SketchPlane3d properties) =
    properties.originPoint


{-| Get the X direction of a sketch plane (the direction of the sketch plane's
X axis).

    SketchPlane3d.xDirection SketchPlane3d.zx
    --> Direction3d.z

-}
xDirection : SketchPlane3d -> Direction3d
xDirection (SketchPlane3d properties) =
    properties.xDirection


{-| Get the Y direction of a sketch plane (the direction of the sketch plane's
Y axis).

    SketchPlane3d.yDirection SketchPlane3d.zx
    --> Direction3d.x

-}
yDirection : SketchPlane3d -> Direction3d
yDirection (SketchPlane3d properties) =
    properties.yDirection


{-| Get the normal direction to a sketch plane. This is equal to the cross
product of the sketch plane's X and Y directions.

    SketchPlane3d.normalDirection SketchPlane3d.xy
    --> Direction3d.z

    SketchPlane3d.normalDirection SketchPlane3d.xz
    --> Direction3d.negativeY

-}
normalDirection : SketchPlane3d -> Direction3d
normalDirection sketchPlane =
    let
        normalVector =
            Vector3d.crossProduct
                (Direction3d.toVector (xDirection sketchPlane))
                (Direction3d.toVector (yDirection sketchPlane))
    in
        Direction3d (Vector3d.components normalVector)


{-| Get the X axis of a sketch plane. A 2D X coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.xAxis SketchPlane3d.zx
    --> Axis3d.z

-}
xAxis : SketchPlane3d -> Axis3d
xAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = xDirection sketchPlane
        }


{-| Get the Y axis of a sketch plane. A 2D Y coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.yAxis SketchPlane3d.zx
    --> Axis3d.x

-}
yAxis : SketchPlane3d -> Axis3d
yAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = yDirection sketchPlane
        }


{-| Get the normal axis to a sketch plane (the axis formed from the sketch
plane's origin point and normal direction).

    SketchPlane3d.normalAxis SketchPlane3d.xy
    --> Axis3d.z

    SketchPlane3d.normalAxis SketchPlane3d.xz
    --> Axis3d.flip Axis3d.y

-}
normalAxis : SketchPlane3d -> Axis3d
normalAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = normalDirection sketchPlane
        }


{-| Conver a `SketchPlane3d` to a `Plane3d` with the same origin point and
normal direction.

    SketchPlane3d.plane SketchPlane3d.xy
    --> Plane3d.xy

    SketchPlane3d.plane SketchPlane3d.yx
    --> Plane3d.flip Plane3d.xy

-}
plane : SketchPlane3d -> Plane3d
plane sketchPlane =
    Plane3d
        { originPoint = originPoint sketchPlane
        , normalDirection = normalDirection sketchPlane
        }


{-| Flip the X direction of a sketch plane, leaving its Y direction and origin
point unchanged.

    SketchPlane3d.flipX SketchPlane3d.yz
    --> SketchPlane3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.negativeY
    -->     , yDirection = Direction3d.positiveZ
    -->     }

-}
flipX : SketchPlane3d -> SketchPlane3d
flipX sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.flip (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


{-| Flip the Y direction of a sketch plane, leaving its X direction and origin
point unchanged.

    SketchPlane3d.flipY SketchPlane3d.yz
    --> SketchPlane3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.positiveY
    -->     , yDirection = Direction3d.negativeZ
    -->     }

-}
flipY : SketchPlane3d -> SketchPlane3d
flipY sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.flip (yDirection sketchPlane)
        }


{-| Set the origin point of the given sketch plane to the given point, leaving
its X and Y directions unchanged.

    SketchPlane3d.moveTo (Point3d ( 2, 1, 3 )) SketchPlane3d.yz
    --> SketchPlane3d
    -->     { originPoint = Point3d ( 2, 1, 3 )
    -->     , xDirection = Direction3d.y
    -->     , yDirection = Direction3d.z
    -->     }

-}
moveTo : Point3d -> SketchPlane3d -> SketchPlane3d
moveTo newOrigin sketchPlane =
    SketchPlane3d
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Rotate a sketch plane around an axis by a given angle (in radians). The
sketch plane's origin point and X and Y directions will all be rotated around
the given axis.

    SketchPlane3d.rotateAround Axis3d.x (degrees 90) SketchPlane3d.xy
    --> SketchPlane3d.xz

-}
rotateAround : Axis3d -> Float -> SketchPlane3d -> SketchPlane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = rotatePoint (originPoint sketchPlane)
                , xDirection = rotateDirection (xDirection sketchPlane)
                , yDirection = rotateDirection (yDirection sketchPlane)
                }


{-| Rotate a sketch plane around one of its own axes by a given angle (in
radians).

The first argument is a function that returns the axis to rotate around, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing sketch planes via a series of
transformations. For example,

    SketchPlane3d.xy
        |> SketchPlane3d.translateBy (Vector3d ( 1, 0, 0 ))
        |> SketchPlane3d.rotateAroundOwn SketchPlane3d.yAxis (degrees -45)
    --> SketchPlane3d
    -->     { originPoint = Point3d ( 1, 0, 0 )
    -->     , xDirection = Direction3d ( 0.7071, 0, 0.7071 )
    -->     , yDirection = Direction3d.y
    -->     }

Note that since the rotation was around the sketch plane's own Y axis (which
passes through the sketch plane's origin point) instead of the global Y axis,
the origin point itself was not affected by the rotation.

-}
rotateAroundOwn :
    (SketchPlane3d -> Axis3d)
    -> Float
    -> SketchPlane3d
    -> SketchPlane3d
rotateAroundOwn axis angle sketchPlane =
    rotateAround (axis sketchPlane) angle sketchPlane


{-| Translate a sketch plane by a given displacement.

    displacement =
        Vector3d ( 2, 1, 3 )

    SketchPlane3d.translateBy displacement SketchPlane3d.xy
    --> SketchPlane3d
    -->     { originPoint = Point3d ( 2, 1, 3 )
    -->     , xDirection = Direction3d.x
    -->     , yDirection = Direction3d.y
    -->     }

-}
translateBy : Vector3d -> SketchPlane3d -> SketchPlane3d
translateBy vector sketchPlane =
    SketchPlane3d
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Translate a sketch plane along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    SketchPlane3d.xy
        |> SketchPlane3d.rotateAround Axis3d.x (degrees 45)
        |> SketchPlane3d.translateAlongOwn SketchPlane3d.yAxis 2

means 'take the global XY sketch plane, rotate it around the global X axis by
45 degrees, then translate the result 2 units along its own (rotated) Y axis',
resulting in

    SketchPlane3d
        { originPoint = Point3d ( 0, 1.4142, 1.4142 )
        , xDirection = Direction3d.x
        , yDirection = Direction3d ( 0, 0.7071, 0.7071 )
        }

-}
translateAlongOwn :
    (SketchPlane3d -> Axis3d)
    -> Float
    -> SketchPlane3d
    -> SketchPlane3d
translateAlongOwn axis distance frame =
    let
        direction =
            Axis3d.direction (axis frame)
    in
        translateBy (Vector3d.in_ direction distance) frame


{-| Mirror a sketch plane across a plane.

    sketchPlane =
        SketchPlane3d
            { originPoint = Point2d ( 2, 1, 3 )
            , xDirection = Direction3d.positiveY
            , yDirection = Direction3d.positiveZ
            }

    SketchPlane3d.mirrorAcross Plane3d.xy sketchPlane
    --> SketchPlane3d
    -->     { originPoint = Point2d ( 2, 1, -3 )
    -->     , xDirection = Direction3d.positiveY
    -->     , yDirection = Direction3d.negativeZ
    -->     }

-}
mirrorAcross : Plane3d -> SketchPlane3d -> SketchPlane3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = mirrorPoint (originPoint sketchPlane)
                , xDirection = mirrorDirection (xDirection sketchPlane)
                , yDirection = mirrorDirection (yDirection sketchPlane)
                }


{-| Take a sketch plane defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d -> SketchPlane3d -> SketchPlane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = relativePoint (originPoint sketchPlane)
                , xDirection = relativeDirection (xDirection sketchPlane)
                , yDirection = relativeDirection (yDirection sketchPlane)
                }


{-| Take a sketch plane defined in local coordinates relative to a given
reference frame, and return that sketch plane expressed in global coordinates.
-}
placeIn : Frame3d -> SketchPlane3d -> SketchPlane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = placePoint (originPoint sketchPlane)
                , xDirection = placeDirection (xDirection sketchPlane)
                , yDirection = placeDirection (yDirection sketchPlane)
                }
