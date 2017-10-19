module OpenSolid.Sphere3d
    exposing
        ( Sphere3d
        , around
        , boundingBox
        , centerPoint
        , circumference
        , contains
        , diameter
        , mirrorAcross
        , placeIn
        , projectInto
        , projectOnto
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , throughPoints
        , translateBy
        , unit
        , volume
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/sphere3d.svg" alt="Sphere3d" width="160">

A `Sphere3d` is defined by its center point, axial direction and radius.
This module contains functionality for:

  - Constructing spheres around axes, or through points
  - Scaling, rotating and translating spheres
  - Extracting sphere properties like center point and volume

@docs Sphere3d


# Constants

@docs unit


# Constructors

@docs with, around, throughPoints


# Properties

@docs centerPoint, radius, diameter, volume, circumference, boundingBox


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn, projectOnto, projectInto

-}

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal exposing (Sphere3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias Sphere3d =
    Internal.Sphere3d


{-| The unit sphere, centered on the origin with a radius of 1.

    Sphere3d.unit
    --> Sphere3d.with
    -->     { centerPoint = Point3d.origin
    -->     , radius = 1
    -->     }

-}
unit : Sphere3d
unit =
    with { centerPoint = Point3d.origin, radius = 1 }


{-| Construct a sphere from its center point and radius:

    exampleSphere =
        Sphere3d.with
            { centerPoint =
                Point3d.fromCoordinates ( 1, 2, 1 )
            , radius = 3
            }

-}
with : { centerPoint : Point3d, radius : Float } -> Sphere3d
with properties =
    Internal.Sphere3d { properties | radius = abs properties.radius }


{-| Construct a sphere around the given axis that passes through the given point.

    point =
        Point3d.fromCoordinates ( 3, 0, 2 )

    Sphere3d.around Axis3d.z point
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 0, 0, 2 )
    -->     , radius = 3
    -->     }

-}
around : Axis3d -> Point3d -> Sphere3d
around axis point =
    let
        centerPoint =
            Point3d.projectOntoAxis axis point
    in
    with
        { centerPoint = centerPoint
        , radius = Point3d.distanceFrom centerPoint point
        }


{-| -}
throughPoints : ( Point3d, Point3d, Point3d, Point3d ) -> Maybe Sphere3d
throughPoints ( p1, p2, p3, p4 ) =
    Maybe.map2
        {-
           First three points define a circle with center `center`.
           This circle lies in the plane `plane`.
           All points M on the normal to `plane` through `center` are equidistant to p1, p2 and p3.
           Therefore: find this point M such that it is equidistant to p1, p2, p3 and p4.
        -}
        (\center plane ->
            let
                normalAxis =
                    Plane3d.moveTo center plane |> Plane3d.normalAxis

                x3s =
                    Point3d.squaredDistanceFrom center p1

                x4 =
                    Point3d.distanceFromAxis normalAxis p4

                y4 =
                    Point3d.distanceAlong normalAxis p4

                d =
                    (x3s ^ 2 - y4 ^ 2 - x4 ^ 2) / (-2 * y4)
            in
            with
                { centerPoint = Point3d.along normalAxis d
                , radius = sqrt (x3s ^ 2 + d ^ 2)
                }
        )
        (Point3d.circumcenter ( p1, p2, p3 ))
        (Plane3d.throughPoints ( p1, p2, p3 ))


{-| Get the center point of a sphere.

    Sphere3d.centerPoint exampleSphere
    --> Point3d.fromCoordinates ( 1, 2, 1 )

-}
centerPoint : Sphere3d -> Point3d
centerPoint (Internal.Sphere3d properties) =
    properties.centerPoint


{-| Get the radius of a sphere.

    Sphere3d.radius exampleSphere
    --> 3

-}
radius : Sphere3d -> Float
radius (Internal.Sphere3d properties) =
    properties.radius


{-| Get the diameter of a sphere.

    Sphere3d.diameter exampleShere
    --> 6

-}
diameter : Sphere3d -> Float
diameter sphere =
    2 * radius sphere


{-| Get the circumference of a sphere.

    Sphere.circumference exampleSphere
    --> 18.8496

-}
circumference : Sphere3d -> Float
circumference sphere =
    2 * pi * radius sphere


{-| Get the volume of a sphere.

    Sphere3d.diameter exampleShere
    --> 6

-}
volume : Sphere3d -> Float
volume sphere =
    4 / 3 * pi * radius sphere ^ 3


{-| Scale a sphere around a given point by a given scale.

    Sphere3d.scaleAbout Point3d.origin 3 exampleSphere
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 3, 6, 3 )
    -->     , radius = 9
    -->     }

-}
scaleAbout : Point3d -> Float -> Sphere3d -> Sphere3d
scaleAbout point scale sphere =
    with
        { centerPoint = Point3d.scaleAbout point scale (centerPoint sphere)
        , radius = abs (scale * radius sphere)
        }


{-| Rotate a sphere around a given axis by a given angle (in radians).

    exampleSphere
        |> Sphere3d.rotateAround Axis3d.y (degrees 90)
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 1, 2, -1)
    -->     , radius = 3
    -->     }

-}
rotateAround : Axis3d -> Float -> Sphere3d -> Sphere3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
    \sphere ->
        with
            { centerPoint = rotatePoint (centerPoint sphere)
            , radius = radius sphere
            }


{-| Translate a sphere by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 1, 3 )

    Sphere3d.translateBy displacement exampleSphere
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 4 )
    -->     , radius = 3
    -->     }

-}
translateBy : Vector3d -> Sphere3d -> Sphere3d
translateBy displacement sphere =
    with
        { centerPoint = Point3d.translateBy displacement (centerPoint sphere)
        , radius = radius sphere
        }


{-| Mirror a sphere across a given plane.

    Sphere3d.mirrorAcross Plane3d.xy exampleSphere
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 2, 0, -1 )
    -->     , axialDirection = Direction3d.negativeZ
    -->     , radius = 3
    -->     }

-}
mirrorAcross : Plane3d -> Sphere3d -> Sphere3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane
    in
    \sphere ->
        with
            { centerPoint = mirrorPoint (centerPoint sphere)
            , radius = radius sphere
            }


{-| Take a sphere defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Sphere3d.relativeTo localFrame exampleSphere
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 0, 0, -2 )
    -->     , radius = 3
    -->     }

-}
relativeTo : Frame3d -> Sphere3d -> Sphere3d
relativeTo frame sphere =
    with
        { centerPoint = Point3d.relativeTo frame (centerPoint sphere)
        , radius = radius sphere
        }


{-| Take a sphere considered to be defined in local coordinates relative to a
given reference frame, and return that sphere expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Sphere3d.placeIn localFrame exampleSphere
    --> Sphere3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 2, 4, 4 )
    -->     , radius = 3
    -->     }

-}
placeIn : Frame3d -> Sphere3d -> Sphere3d
placeIn frame sphere =
    with
        { centerPoint = Point3d.placeIn frame (centerPoint sphere)
        , radius = radius sphere
        }


{-| Get the minimal bounding box containing a given sphere.

    Sphere3d.boundingBox exampleSphere
    --> BoundingBox3d.with
    -->     { minX = -1
    -->     , maxX = 5
    -->     , minY = -3
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 1
    -->     }

-}
boundingBox : Sphere3d -> BoundingBox3d
boundingBox sphere =
    let
        r =
            radius sphere

        ( cx, cy, cz ) =
            Plane3d
                Point3d.coordinates
                (centerPoint sphere)
    in
    BoundingBox3d.with
        { minX = cx - r
        , maxX = cx + r
        , minY = cy - r
        , maxY = cy + r
        , minZ = cz - r
        , maxZ = cz + r
        }


{-| Check if a sphere contains a given point.
-}
contains : Point3d -> Sphere3d -> Bool
contains point sphere =
    Point3d.squaredDistanceFrom point (centerPoint sphere) <= radius sphere ^ 2


{-| Project a sphere orthographically (perpendicularly) onto a plane.
-}
projectOnto : Plane3d -> Sphere3d -> Circle3d
projectOnto plane sphere =
    Circle3d.with
        { centerPoint = Point3d.projectOnto plane (centerPoint sphere)
        , axialDirection = Plane3d.normalDirection plane
        , radius = Sphere3d.radius sphere
        }


{-| Project a sphere orhtographically (perpendicularly) into a sketch plane.
-}
projectInto : SketchPlane3d -> Sphere3d -> Circle2d
projectInto sketchPlane sphere =
    Circle2d.with
        { centerPoint = Point3d.projectInto sketchPlane (centerPoint sphere)
        , radius = Sphere3d.radius sphere
        }
