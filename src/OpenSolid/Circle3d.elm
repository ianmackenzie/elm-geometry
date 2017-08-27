module OpenSolid.Circle3d
    exposing
        ( Circle3d
        , area
        , around
        , axialDirection
        , boundingBox
        , centerPoint
        , circumference
        , diameter
        , mirrorAcross
        , on
        , placeIn
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , through
        , translateBy
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/circle3d.svg" alt="Circle3d" width="160">

A `Circle3d` is defined by its center point, axial direction and radius. The
axial direction is the direction of the axis through the center of the circle
that all points on the circle are equidistant from, or equivalently the normal
direction of the plane defined by the circle. This module contains functionality
for:

  - Constructing circles around axes, on planes, or through points
  - Scaling, rotating and translating circles
  - Extracting circle properties like center point and area

@docs Circle3d


# Constructors

@docs with, around, on, through


# Accessors

@docs centerPoint, axialDirection, radius, diameter, area, circumference


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Bounds

@docs boundingBox

-}

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias Circle3d =
    Internal.Circle3d


{-| Construct a circle from its center point, axial direction and radius:

    exampleCircle =
        Circle3d.with
            { centerPoint = Point3d.withCoordinates ( 2, 0, 1 )
            , axialDirection = Direction3d.z
            , radius = 3
            }

The actual radius of the circle will be the absolute value of the given radius
(passing `radius = -2` will have the same effect as `radius = 2`).

-}
with : { centerPoint : Point3d, axialDirection : Direction3d, radius : Float } -> Circle3d
with properties =
    Internal.Circle3d { properties | radius = abs properties.radius }


{-| Construct a circle centered on the given axis that passes through the given
point.

    point =
        Point3d.withCoordinates ( 3, 0, 2 )

    Circle3d.around Axis3d.z point
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 0, 0, 2 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
around : Axis3d -> Point3d -> Circle3d
around axis point =
    let
        centerPoint =
            Point3d.projectOntoAxis axis point
    in
    with
        { centerPoint = centerPoint
        , axialDirection = Axis3d.direction axis
        , radius = Point3d.distanceFrom centerPoint point
        }


{-| Construct a 3D circle lying _on_ a sketch plane by providing a 2D circle
specified in XY coordinates _within_ the sketch plane.

    Circle3d.on SketchPlane3d.yz <|
        Circle2d.with
            { centerPoint = Point2d.withCoordinates ( 1, 2 )
            , radius = 3
            }
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 0, 1, 2 )
    -->     , axialDirection = Direction3d.x
    -->     , radius = 3
    -->     }

-}
on : SketchPlane3d -> Circle2d -> Circle3d
on sketchPlane circle =
    with
        { centerPoint = Point3d.on sketchPlane (Circle2d.centerPoint circle)
        , axialDirection = SketchPlane3d.normalDirection sketchPlane
        , radius = Circle2d.radius circle
        }


{-| Attempt to construct a circle that passes through the three given points. If
the three given points are collinear, returns `Nothing`.

    Circle3d.through
        ( Point3d.withCoordinates ( 1, 0, 0 )
        , Point3d.withCoordinates ( 0, 1, 0 )
        , Point3d.withCoordinates ( 0, 0, 1 )
        )
    --> Just
    -->     (Circle3d.with
    -->         { centerPoint =
    -->             Point3d.withCoordinates ( 0.333, 0.333, 0.333 )
    -->         , axialDirection =
    -->             Direction3d.with
    -->                 { azimuth = degrees 45
    -->                 , elevation = degrees 35.26
    -->                 }
    -->         , radius = 0.8165
    -->         }
    -->     )

-}
through : ( Point3d, Point3d, Point3d ) -> Maybe Circle3d
through points =
    Maybe.map2
        (\centerPoint plane ->
            let
                ( p1, p2, p3 ) =
                    points

                r1 =
                    Point3d.distanceFrom centerPoint p1

                r2 =
                    Point3d.distanceFrom centerPoint p2

                r3 =
                    Point3d.distanceFrom centerPoint p3
            in
            with
                { centerPoint = centerPoint
                , axialDirection = Plane3d.normalDirection plane
                , radius = (r1 + r2 + r3) / 3
                }
        )
        (Point3d.circumcenter points)
        (Plane3d.through points)


{-| Get the center point of a circle.

    Circle3d.centerPoint exampleCircle
    --> Point3d.withCoordinates ( 2, 0, 1 )

-}
centerPoint : Circle3d -> Point3d
centerPoint (Internal.Circle3d properties) =
    properties.centerPoint


{-| Get the axial direction of a circle.

    Circle3d.axialDirection exampleCircle
    --> Direction3d.z

-}
axialDirection : Circle3d -> Direction3d
axialDirection (Internal.Circle3d properties) =
    properties.axialDirection


{-| Get the radius of a circle.

    Circle3d.radius exampleCircle
    --> 3

-}
radius : Circle3d -> Float
radius (Internal.Circle3d properties) =
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
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 6, 0, 3 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 9
    -->     }

-}
scaleAbout : Point3d -> Float -> Circle3d -> Circle3d
scaleAbout point scale circle =
    with
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
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 1, 0, -2 )
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
        with
            { centerPoint = rotatePoint (centerPoint circle)
            , radius = radius circle
            , axialDirection = rotateDirection (axialDirection circle)
            }


{-| Translate a circle by a given displacement.

    displacement =
        Vector3d.withComponents ( 2, 1, 3 )

    Circle3d.translateBy displacement exampleCircle
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 4, 1, 4 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
translateBy : Vector3d -> Circle3d -> Circle3d
translateBy displacement circle =
    with
        { centerPoint = Point3d.translateBy displacement (centerPoint circle)
        , radius = radius circle
        , axialDirection = axialDirection circle
        }


{-| Mirror a circle across a given plane.

    Circle3d.mirrorAcross Plane3d.xy exampleCircle
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 2, 0, -1 )
    -->     , axialDirection = Direction3d.negativeZ
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
        with
            { centerPoint = mirrorPoint (centerPoint circle)
            , radius = radius circle
            , axialDirection = mirrorDirection (axialDirection circle)
            }


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    Circle3d.relativeTo localFrame exampleCircle
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 1, -2, -2 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
relativeTo : Frame3d -> Circle3d -> Circle3d
relativeTo frame circle =
    with
        { centerPoint = Point3d.relativeTo frame (centerPoint circle)
        , radius = radius circle
        , axialDirection = Direction3d.relativeTo frame (axialDirection circle)
        }


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    Circle3d.placeIn localFrame exampleCircle
    --> Circle3d.with
    -->     { centerPoint = Point3d.withCoordinates ( 3, 2, 4 )
    -->     , axialDirection = Direction3d.z
    -->     , radius = 3
    -->     }

-}
placeIn : Frame3d -> Circle3d -> Circle3d
placeIn frame circle =
    with
        { centerPoint = Point3d.placeIn frame (centerPoint circle)
        , radius = radius circle
        , axialDirection = Direction3d.placeIn frame (axialDirection circle)
        }


{-| Get the minimal bounding box containing a given circle.

    Circle3d.boundingBox exampleCircle
    --> BoundingBox3d.with
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
    BoundingBox3d.with
        { minX = cx - dx
        , maxX = cx + dx
        , minY = cy - dy
        , maxY = cy + dy
        , minZ = cz - dz
        , maxZ = cz + dz
        }
