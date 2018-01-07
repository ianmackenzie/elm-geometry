module OpenSolid.Sphere3d
    exposing
        ( Sphere3d
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
        , surfaceArea
        , throughPoints
        , translateBy
        , unit
        , volume
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/sphere3d.svg" alt="Sphere3d" width="160">

A `Sphere3d` is defined by its center point and radius. This module contains
functionality for:

  - Constructing spheres through points
  - Scaling, rotating and translating spheres
  - Extracting sphere properties like center point and volume

@docs Sphere3d


# Constants

@docs unit


# Constructors

@docs with, throughPoints


# Properties

@docs centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Circle3d as Circle3d exposing (Circle3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal exposing (Sphere3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{--Imports for verifying the examples:

    import OpenSolid.Geometry.Examples.Sphere3d exposing (..)
    import OpenSolid.Geometry.Examples.Expect as Expect
    import OpenSolid.Axis3d as Axis3d
    import OpenSolid.BoundingBox3d as BoundingBox3d
    import OpenSolid.Circle2d as Circle2d
    import OpenSolid.Circle3d as Circle3d
    import OpenSolid.Direction3d as Direction3d
    import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
    import OpenSolid.Plane3d as Plane3d
    import OpenSolid.Point2d as Point2d
    import OpenSolid.Point3d as Point3d exposing (Point3d)
    import OpenSolid.Scalar as Scalar
    import OpenSolid.SketchPlane3d as SketchPlane3d
    import OpenSolid.Sphere3d as Sphere3d
    import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
-}


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

A negative radius will be interpreted as positive (the absolute value will be
used).

-}
with : { centerPoint : Point3d, radius : Float } -> Sphere3d
with properties =
    Internal.Sphere3d { properties | radius = abs properties.radius }


{-| Attempt to construct a sphere that passes through the four given points.
Returns `Nothing` if four given points are coplanar.

    Sphere3d.throughPoints
        ( Point3d.fromCoordinates ( 1, 0, 0 )
        , Point3d.fromCoordinates ( -1, 0, 0 )
        , Point3d.fromCoordinates ( 0, 1, 0 )
        , Point3d.fromCoordinates ( 0, 0, 0.5 )
        )
    --> Just <| Sphere3d.with
        { centerPoint =
            Point3d.fromCoordinates ( 0, 0, -0.75 )
            , radius = 1.25
            }

    Sphere3d.throughPoints
        ( Point3d.fromCoordinates ( 1, 0, 0 )
        , Point3d.fromCoordinates ( -1, 0, 0 )
        , Point3d.fromCoordinates ( 0, 1, 0 )
        , Point3d.fromCoordinates ( 0, -1, 0 )
        )
    --> Nothing

-}
throughPoints : ( Point3d, Point3d, Point3d, Point3d ) -> Maybe Sphere3d
throughPoints ( p1, p2, p3, p4 ) =
    Circle3d.throughPoints ( p1, p2, p3 )
        |> Maybe.andThen
            {-
               First three points define a circle.
               All points M on the normal to this circle through the circle's center are equidistant to p1, p2 and p3.
               Therefore: find this point M such that it is equidistant to p1, p2, p3 and p4,
               this will be the center of the sphere.
            -}
            (\circle ->
                let
                    normalAxis =
                        Circle3d.axis circle

                    r =
                        Circle3d.radius circle

                    x =
                        Point3d.distanceFromAxis normalAxis p4

                    y =
                        Point3d.signedDistanceAlong normalAxis p4
                in
                if y /= 0 then
                    let
                        d =
                            (r * r - x * x - y * y) / (-2 * y)
                    in
                    Just <|
                        with
                            { centerPoint = Point3d.along normalAxis d
                            , radius = sqrt (r * r + d * d)
                            }
                else
                    Nothing
            )


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

    Sphere3d.diameter exampleSphere
    --> 6

-}
diameter : Sphere3d -> Float
diameter sphere =
    2 * radius sphere


{-| Get the circumference of a sphere (the circumference of a [great circle](https://en.wikipedia.org/wiki/Great_circle)
of the sphere).

    Sphere3d.circumference exampleSphere
    --> 18.8496

-}
circumference : Sphere3d -> Float
circumference sphere =
    2 * pi * radius sphere


{-| Get the surface area of a sphere.

    Sphere3d.surfaceArea exampleSphere
    --> 113.0973

-}
surfaceArea : Sphere3d -> Float
surfaceArea sphere =
    let
        r =
            radius sphere
    in
    4 * pi * r * r


{-| Get the volume of a sphere.

    Sphere3d.volume exampleSphere
    --> 113.0973

-}
volume : Sphere3d -> Float
volume sphere =
    let
        r =
            radius sphere
    in
    4 / 3 * pi * r * r * r


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
    -->         Point3d.fromCoordinates ( 1, 2, -1 )
    -->     , radius = 3
    -->     }

-}
rotateAround : Axis3d -> Float -> Sphere3d -> Sphere3d
rotateAround axis angle sphere =
    let
        rotatePoint =
            Point3d.rotateAround axis angle
    in
    with
        { centerPoint = rotatePoint (centerPoint sphere)
        , radius = radius sphere
        }


{-| Translate a sphere by a given displacement.

    displacement : Vector3d
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
    -->         Point3d.fromCoordinates ( 1, 2, -1 )
    -->     , radius = 3
    -->     }

-}
mirrorAcross : Plane3d -> Sphere3d -> Sphere3d
mirrorAcross plane sphere =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane
    in
    with
        { centerPoint = mirrorPoint (centerPoint sphere)
        , radius = radius sphere
        }


{-| Take a sphere defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame : Frame3d
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

    localFrame : Frame3d
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
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 5
    -->     , minZ = -2
    -->     , maxZ = 4
    -->     }

-}
boundingBox : Sphere3d -> BoundingBox3d
boundingBox sphere =
    let
        r =
            radius sphere

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint sphere)
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

    Sphere3d.contains
        (Point3d.fromCoordinates ( 4, 2, 1 ))
        exampleSphere
    --> True

    Sphere3d.contains
        (Point3d.fromCoordinates ( 4.00001, 2, 1 ))
        exampleSphere
    --> False

-}
contains : Point3d -> Sphere3d -> Bool
contains point sphere =
    let
        r =
            radius sphere
    in
    Point3d.squaredDistanceFrom (centerPoint sphere) point <= r * r


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere onto a plane.

    Sphere3d.projectOnto Plane3d.xy exampleSphere
    --> Circle3d.with
    -->     { centerPoint =
    -->         Point3d.fromCoordinates ( 1, 2, 0 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3.0
    -->     }

-}
projectOnto : Plane3d -> Sphere3d -> Circle3d
projectOnto plane sphere =
    Circle3d.with
        { centerPoint = Point3d.projectOnto plane (centerPoint sphere)
        , axialDirection = Plane3d.normalDirection plane
        , radius = radius sphere
        }


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere into a sketch plane.

    Sphere3d.projectInto SketchPlane3d.xy exampleSphere
    --> Circle2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , radius = 3.0
    -->     }

-}
projectInto : SketchPlane3d -> Sphere3d -> Circle2d
projectInto sketchPlane sphere =
    Circle2d.with
        { centerPoint = Point3d.projectInto sketchPlane (centerPoint sphere)
        , radius = radius sphere
        }
