module OpenSolid.Circle2d
    exposing
        ( unit
        , throughPoints
        , centerPoint
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
        , placeOnto
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle2d.svg" alt="Circle2d" width="160">

A `Circle2d` is defined by its center point and radius. Currently you can only
do a few basic things with circles, such as measuring the area or checking if a
particular point is contained, but this should increase in the future.

Circles can be constructed by passing a record with `centerPoint` and `radius`
fields to the `Circle2d` constructor, for example

    exampleCircle =
        Circle2d
            { centerPoint = Point2d ( 1, 2 )
            , radius = 3
            }

**You must ensure the provided radius is positive** (or zero, but that's not a
very useful circle).


# Predefined circles

@docs unit


# Constructors

@docs throughPoints


# Accessors

@docs centerPoint, radius, diameter, area, circumference


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.SketchPlane3d as SketchPlane3d


{-| The unit circle, centered on the origin with a radius of 1.

    Circle2d.unit
    --> Circle2d
    -->     { centerPoint = Point2d.origin
    -->     , radius = 1
    -->     }

-}
unit : Circle2d
unit =
    Circle2d { centerPoint = Point2d.origin, radius = 1 }


{-| Attempt to construct a circle that passes through the three given points. If
the three given points are collinear, returns `Nothing`.

    Circle2d.throughPoints
        Point2d.origin
        (Point2d ( 1, 0 ))
        (Point2d ( 0, 1 ))
    --> Just
    -->     (Circle2d
    -->         { centerPoint = Point2d ( 0.5, 0.5 )
    -->         , radius = 0.7071
    -->         }
    -->     )

    Circle2d.throughPoints
        Point2d.origin
        (Point2d ( 2, 1 ))
        (Point2d ( 4, 0 ))
    --> Just
    -->     (Circle2d
    -->         { centerPoint = Point2d ( 2, -1.5 )
    -->         , radius = 2.5
    -->         }
    -->     )

    Circle2d.throughPoints
        Point2d.origin
        (Point2d ( 2, 0 ))
        (Point2d ( 4, 0 ))
    --> Nothing

    Circle2d.throughPoints
        Point2d.origin
        Point2d.origin
        (Point2d ( 1, 0 ))
    --> Nothing

-}
throughPoints : Point2d -> Point2d -> Point2d -> Maybe Circle2d
throughPoints firstPoint secondPoint thirdPoint =
    let
        a2 =
            Point2d.squaredDistanceFrom firstPoint secondPoint

        b2 =
            Point2d.squaredDistanceFrom secondPoint thirdPoint

        c2 =
            Point2d.squaredDistanceFrom thirdPoint firstPoint

        t1 =
            a2 * (b2 + c2 - a2)

        t2 =
            b2 * (c2 + a2 - b2)

        t3 =
            c2 * (a2 + b2 - c2)

        sum =
            t1 + t2 + t3
    in
        if sum == 0 then
            Nothing
        else
            let
                w1 =
                    t1 / sum

                w2 =
                    t2 / sum

                w3 =
                    t3 / sum

                ( x1, y1 ) =
                    Point2d.coordinates firstPoint

                ( x2, y2 ) =
                    Point2d.coordinates secondPoint

                ( x3, y3 ) =
                    Point2d.coordinates thirdPoint

                centerPoint =
                    Point2d
                        ( w1 * x3 + w2 * x1 + w3 * x2
                        , w1 * y3 + w2 * y1 + w3 * y2
                        )

                r1 =
                    Point2d.distanceFrom centerPoint firstPoint

                r2 =
                    Point2d.distanceFrom centerPoint secondPoint

                r3 =
                    Point2d.distanceFrom centerPoint thirdPoint
            in
                Just
                    (Circle2d
                        { centerPoint = centerPoint
                        , radius = (r1 + r2 + r3) / 3
                        }
                    )


{-| Get the center point of a circle.

    Circle2d.centerPoint exampleCircle
    --> Point2d ( 1, 2 )

-}
centerPoint : Circle2d -> Point2d
centerPoint (Circle2d properties) =
    properties.centerPoint


{-| Get the radius of a circle.

    Circle2d.radius exampleCircle
    --> 3

-}
radius : Circle2d -> Float
radius (Circle2d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circle2d.diameter exampleCircle
    --> 6

-}
diameter : Circle2d -> Float
diameter circle =
    2 * radius circle


{-| Get the area of a circle.

    Circle2d.area exampleCircle
    --> 28.2743

-}
area : Circle2d -> Float
area circle =
    let
        r =
            radius circle
    in
        pi * r * r


{-| Get the circumference of a circle.

    Circle2d.circumference exampleCircle
    --> 18.8496

-}
circumference : Circle2d -> Float
circumference circle =
    2 * pi * radius circle


{-| Check if a circle contains a given point.

    Circle2d.contains Point2d.origin exampleCircle
    --> True

    Circle2d.contains (Point2d ( 10, 10 )) exampleCircle
    --> False

-}
contains : Point2d -> Circle2d -> Bool
contains point circle =
    let
        r =
            radius circle
    in
        Point2d.squaredDistanceFrom (centerPoint circle) point <= r * r


{-| Scale a circle about a given point by a given scale.

    Circle2d.scaleAbout Point2d.origin 2 exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( 2, 4 )
    -->     , radius = 6
    -->     }

    Circle2d.scaleAbout (Point2d ( 1, 2 )) 0.5 exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( 1, 2 )
    -->     , radius = 1.5
    -->     }

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

    Circle2d.rotateAround Point2d.origin (degrees 90) exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( -2, 1 )
    -->     , radius = 3
    -->     }

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

    Circle2d.translateBy (Vector2d ( 2, 2 )) exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( 3, 4 )
    -->     , radius = 3
    -->     }

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

    Circle2d.mirrorAcross Axis2d.x exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( 1, -2 )
    -->     , radius = 3
    -->     }

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

    Circle2d.relativeTo localFrame exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( -1, -1 )
    -->     , radius = 3
    -->     }

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

    Circle2d.placeIn localFrame exampleCircle
    --> Circle2d
    -->     { centerPoint = Point2d ( 3, 5 )
    -->     , radius = 3
    -->     }

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


{-| Take a circle defined in 2D coordinates within a particular sketch plane and
return the corresponding circle in 3D.

    Circle2d.placeOnto SketchPlane3d.yz exampleCircle
    --> Circle3d
    -->     { centerPoint = Point3d ( 0, 1, 2 )
    -->     , axialDirection = Direction3d.x
    -->     , radius = 3
    -->     }

-}
placeOnto : SketchPlane3d -> Circle2d -> Circle3d
placeOnto sketchPlane circle =
    Circle3d
        { centerPoint = Point2d.placeOnto sketchPlane (centerPoint circle)
        , axialDirection = SketchPlane3d.normalDirection sketchPlane
        , radius = radius circle
        }


{-| Get the minimal bounding box containing a given circle.

    Circle2d.boundingBox exampleCircle
    --> BoundingBox2d
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 5
    -->     }

-}
boundingBox : Circle2d -> BoundingBox2d
boundingBox circle =
    let
        ( x, y ) =
            Point2d.coordinates (centerPoint circle)

        r =
            radius circle
    in
        BoundingBox2d
            { minX = x - r
            , maxX = x + r
            , minY = y - r
            , maxY = y + r
            }
