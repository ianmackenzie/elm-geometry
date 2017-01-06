module OpenSolid.Circle2d
    exposing
        ( centerPoint
        , radius
        , diameter
        , area
        , circumference
        , contains
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle2d.svg" alt="Circle2d" width="160">

Various functions for creating and working with `Circle2d` values. A `Circle2d`
is defined by its center point and radius.

Circles can be constructed by passing a record with `centerPoint` and `radius`
fields to the `Circle2d` constructor, for example

    exampleCircle =
        Circle2d
            { centerPoint = Point2d ( 1, 2 )
            , radius = 3
            }

## Reading this documentation

For the examples below, assume that `exampleCircle` has been defined as above,
all OpenSolid core types have been imported using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.Circle2d as Circle2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Accessors

@docs centerPoint, radius, diameter, area, circumference

# Queries

@docs contains

# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross

# Coordinate transformations

@docs relativeTo, placeIn
-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d


{-| Get the center point of a circle.

    Circle2d.centerPoint exampleCircle ==
        Point2d ( 1, 2 )
-}
centerPoint : Circle2d -> Point2d
centerPoint (Circle2d properties) =
    properties.centerPoint


{-| Get the radius of a circle.

    Circle2d.radius exampleCircle ==
        3
-}
radius : Circle2d -> Float
radius (Circle2d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circle2d.diameter exampleCircle ==
        6
-}
diameter : Circle2d -> Float
diameter circle =
    2 * radius circle


{-| Get the area of a circle.

    Circle2d.area exampleCircle ==
        28.2743
-}
area : Circle2d -> Float
area circle =
    let
        r =
            radius circle
    in
        pi * r * r


{-| Get the circumference of a circle.

    Circle2d.circumference exampleCircle ==
        18.8496

-}
circumference : Circle2d -> Float
circumference circle =
    let
        r =
            radius circle
    in
        2 * pi * r


{-| Check if a circle contains a given point.

    Circle2d.contains Point2d.origin exampleCircle ==
        True

    Circle2d.contains (Point2d ( 10, 10 )) exampleCircle ==
        False
-}
contains : Point2d -> Circle2d -> Bool
contains point circle =
    let
        r =
            radius circle
    in
        Point2d.squaredDistanceFrom (centerPoint circle) point <= r * r


{-| Scale a circle about a given point by a given scale.

    Circle2d.scaleAbout Point2d.origin 2 exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( 2, 4 )
            , radius = 6
            }

    Circle2d.scaleAbout (Point2d ( 1, 2 )) 0.5 exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( 1, 2 )
            , radius = 1.5
            }
-}
scaleAbout : Point2d -> Float -> Circle2d -> Circle2d
scaleAbout point scale =
    let
        scalePoint =
            Point2d.scaleAbout point scale
    in
        \circle ->
            Circle2d
                { centerPoint = scalePoint (centerPoint circle)
                , radius = scale * radius circle
                }


{-| Rotate a circle around a given point by a given angle (in radians).

    Circle2d.rotateAround Point2d.origin (degrees 90) exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( -2, 1 )
            , radius = 3
            }
-}
rotateAround : Point2d -> Float -> Circle2d -> Circle2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
        \circle ->
            Circle2d
                { centerPoint = rotatePoint (centerPoint circle)
                , radius = radius circle
                }


{-| Translate a circle by a given displacement.

    Circle2d.translateBy (Vector2d ( 2, 2 )) exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( 3, 4 )
            , radius = 3
            }
-}
translateBy : Vector2d -> Circle2d -> Circle2d
translateBy displacement =
    let
        translatePoint =
            Point2d.translateBy displacement
    in
        \circle ->
            Circle2d
                { centerPoint = translatePoint (centerPoint circle)
                , radius = radius circle
                }


{-| Mirror a circle across a given axis.

    Circle2d.mirrorAcross Axis2d.x exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( 1, -2 )
            , radius = 3
            }
-}
mirrorAcross : Axis2d -> Circle2d -> Circle2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis
    in
        \circle ->
            Circle2d
                { centerPoint = mirrorPoint (centerPoint circle)
                , radius = radius circle
                }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 2, 3 ))

    Circle2d.relativeTo localFrame exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( -1, -1 )
            , radius = 3
            }
-}
relativeTo : Frame2d -> Circle2d -> Circle2d
relativeTo frame =
    let
        relativePoint =
            Point2d.relativeTo frame
    in
        \circle ->
            Circle2d
                { centerPoint = relativePoint (centerPoint circle)
                , radius = radius circle
                }


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 2, 3 ))

    Circle2d.placeIn localFrame exampleCircle ==
        Circle2d
            { centerPoint = Point2d ( 3, 5 )
            , radius = 3
            }
-}
placeIn : Frame2d -> Circle2d -> Circle2d
placeIn frame =
    let
        placePoint =
            Point2d.placeIn frame
    in
        \circle ->
            Circle2d
                { centerPoint = placePoint (centerPoint circle)
                , radius = radius circle
                }
