module OpenSolid.Circle2d
    exposing
        ( Circle2d
        , area
        , boundingBox
        , centerPoint
        , circumference
        , contains
        , diameter
        , mirrorAcross
        , placeIn
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , throughPoints
        , toArc
        , translateBy
        , unit
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle2d.svg" alt="Circle2d" width="160">

A `Circle2d` is defined by its center point and radius. This module includes
functionality for

  - Constructing circles through points or with a given center/radius
  - Scaling, rotating and translating circles
  - Extracting properties of circles like area, center point and radius

@docs Circle2d


# Constants

@docs unit


# Constructors

@docs with, throughPoints


# Properties

@docs centerPoint, radius, diameter, area, circumference, boundingBox


# Conversion

@docs toArc


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Bootstrap.Arc2d as Arc2d
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal exposing (Arc2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias Circle2d =
    Internal.Circle2d


{-| Construct a circle from its center point and radius:

    exampleCircle =
        Circle2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 1, 2 )
            , radius = 3
            }

The actual radius of the circle will be the absolute value of the given radius
(passing `radius = -2` will have the same effect as `radius = 2`).

-}
with : { centerPoint : Point2d, radius : Float } -> Circle2d
with properties =
    Internal.Circle2d { properties | radius = abs properties.radius }


{-| The unit circle, centered on the origin with a radius of 1.

    Circle2d.unit
    --> Circle2d.with
    -->     { centerPoint = Point2d.origin
    -->     , radius = 1
    -->     }

-}
unit : Circle2d
unit =
    with { centerPoint = Point2d.origin, radius = 1 }


{-| Attempt to construct a circle that passes through the three given points. If
the three given points are collinear, returns `Nothing`.

    Circle2d.throughPoints
        ( Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        , Point2d.fromCoordinates ( 0, 1 )
        )
    --> Just
    -->     (Circle2d.with
    -->         { centerPoint =
    -->             Point2d.fromCoordinates ( 0.5, 0.5 )
    -->         , radius = 0.7071
    -->         }
    -->     )

    Circle2d.throughPoints
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 1 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Just
    -->     (Circle2d.with
    -->         { centerPoint =
    -->             Point2d.fromCoordinates ( 2, -1.5 )
    -->         , radius = 2.5
    -->         }
    -->     )

    Circle2d.throughPoints
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 0 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Nothing

    Circle2d.throughPoints
        ( Point2d.origin
        , Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        )
    --> Nothing

-}
throughPoints : ( Point2d, Point2d, Point2d ) -> Maybe Circle2d
throughPoints points =
    Point2d.circumcenter points
        |> Maybe.map
            (\p0 ->
                let
                    ( p1, p2, p3 ) =
                        points

                    r1 =
                        Point2d.distanceFrom p0 p1

                    r2 =
                        Point2d.distanceFrom p0 p2

                    r3 =
                        Point2d.distanceFrom p0 p3
                in
                with { centerPoint = p0, radius = (r1 + r2 + r3) / 3 }
            )


{-| Get the center point of a circle.

    Circle2d.centerPoint exampleCircle
    --> Point2d.fromCoordinates ( 1, 2 )

-}
centerPoint : Circle2d -> Point2d
centerPoint (Internal.Circle2d properties) =
    properties.centerPoint


{-| Get the radius of a circle.

    Circle2d.radius exampleCircle
    --> 3

-}
radius : Circle2d -> Float
radius (Internal.Circle2d properties) =
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


{-| Convert a circle to a 360 degree arc.

    Circle2d.toArc exampleCircle
    --> Arc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , startPoint =
    -->         Point2d.fromCoordinates ( 4, 2 )
    -->     , sweptAngle = degrees 360
    -->     }

-}
toArc : Circle2d -> Arc2d
toArc (Internal.Circle2d { centerPoint, radius }) =
    let
        ( x0, y0 ) =
            Point2d.coordinates centerPoint
    in
    Arc2d.with
        { centerPoint = centerPoint
        , startPoint = Point2d.fromCoordinates ( x0 + radius, y0 )
        , sweptAngle = 2 * pi
        }


{-| Check if a circle contains a given point.

    Circle2d.contains Point2d.origin exampleCircle
    --> True

    exampleCircle
        |> Circle2d.contains
            (Point2d.fromCoordinates ( 10, 10 ))
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
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 2, 4 )
    -->     , radius = 6
    -->     }

    exampleCircle
        |> Circle2d.scaleAbout
            (Point2d.fromCoordinates ( 1, 2 ))
            0.5
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
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
        with
            { centerPoint = scalePoint (centerPoint circle)
            , radius = scale * radius circle
            }


{-| Rotate a circle around a given point by a given angle (in radians).

    exampleCircle
        |> Circle2d.rotateAround Point2d.origin
            (degrees 90)
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( -2, 1 )
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
        with
            { centerPoint = rotatePoint (centerPoint circle)
            , radius = radius circle
            }


{-| Translate a circle by a given displacement.

    exampleCircle
        |> Circle2d.translateBy
            (Vector2d.fromComponents ( 2, 2 ))
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 3, 4 )
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
        with
            { centerPoint = translatePoint (centerPoint circle)
            , radius = radius circle
            }


{-| Mirror a circle across a given axis.

    Circle2d.mirrorAcross Axis2d.x exampleCircle
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, -2 )
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
        with
            { centerPoint = mirrorPoint (centerPoint circle)
            , radius = radius circle
            }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Circle2d.relativeTo localFrame exampleCircle
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( -1, -1 )
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
        with
            { centerPoint = relativePoint (centerPoint circle)
            , radius = radius circle
            }


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Circle2d.placeIn localFrame exampleCircle
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 3, 5 )
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
        with
            { centerPoint = placePoint (centerPoint circle)
            , radius = radius circle
            }


{-| Get the minimal bounding box containing a given circle.

    Circle2d.boundingBox exampleCircle
    --> BoundingBox2d.with
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
    BoundingBox2d.with
        { minX = x - r
        , maxX = x + r
        , minY = y - r
        , maxY = y + r
        }
