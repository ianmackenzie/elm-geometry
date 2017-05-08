module OpenSolid.Arc3d
    exposing
        ( throughPoints
        , axis
        , centerPoint
        , axialDirection
        , radius
        , startPoint
        , endPoint
        , point
        , sweptAngle
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/arc3d.svg" alt="Arc3d" width="160">

An `Arc3d` is a section of a circle in 3D, defined by its central axis,
start point and swept angle (the counterclockwise angle around the axis from the
start point to the arc's end point). This module includes functionality for

  - Constructing arcs through given points
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

Arcs can be constructed explicitly by passing a record with `axis`, `startPoint`
and `sweptAngle` fields to the `Arc3d` constructor, for example

    exampleArc =
        Arc3d
            { axis = Axis3d.z
            , startPoint = Point3d ( 2, 0, 1 )
            , sweptAngle = degrees 90
            }

Note that the origin point of the axis is not required to be the same as the
arc's center point - here the origin point of the given axis is
`Point3d.origin`, while the center point of the arc is `Point3d ( 0, 0, 1 )`
(the start point of the arc projected onto the axis).


# Constructors

@docs throughPoints


# Accessors

@docs axis, centerPoint, axialDirection, radius, startPoint, endPoint, point, sweptAngle


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Arc2d as Arc2d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.SketchPlane3d as SketchPlane3d


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    p1 =
        Point3d ( 0, 0, 1 )

    p2 =
        Point3d.origin

    p3 =
        Point3d ( 0, 1, 0 )

    Arc3d.throughPoints p1 p2 p3
    --> Just
    -->     (Arc3d
    -->         { axis =
    -->             Axis3d
    -->                 { originPoint = Point3d ( 0, 0.5, 0.5 )
    -->                 , direction = Direction3d ( 1, 0, 0 )
    -->                 }
    -->         , startPoint = Point3d (0,0,1)
    -->         , sweptAngle = 3.1416
    -->         }
    -->     )

-}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe Arc3d
throughPoints firstPoint secondPoint thirdPoint =
    SketchPlane3d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\sketchPlane ->
                let
                    project =
                        Point3d.projectInto sketchPlane
                in
                    Arc2d.throughPoints
                        (project firstPoint)
                        (project secondPoint)
                        (project thirdPoint)
                        |> Maybe.map (Arc2d.placeOnto sketchPlane)
            )


{-| Get the central axis of an arc.

    Arc3d.axis exampleArc
    --> Axis3d.z

-}
axis : Arc3d -> Axis3d
axis (Arc3d properties) =
    properties.axis


{-| Get the center point of an arc.

    Arc3d.centerPoint exampleArc
    --> Point3d ( 0, 0, 1 )

The center point of an arc is equal to its start point projected onto its
central axis, which may not be equal to the origin point of that axis.

-}
centerPoint : Arc3d -> Point3d
centerPoint arc =
    Point3d.projectRadiallyOnto (axis arc) (startPoint arc)


{-| Get direction of an arc's axis;

    Arc3d.axialDirection arc

is equivalent to

    Axis3d.direction (Arc3d.axis arc)

-}
axialDirection : Arc3d -> Direction3d
axialDirection arc =
    Axis3d.direction (axis arc)


{-| Get the radius of an arc.

    Arc3d.radius exampleArc
    --> 2

-}
radius : Arc3d -> Float
radius arc =
    Point3d.radialDistanceFrom (axis arc) (startPoint arc)


{-| Get the start point of an arc.

    Arc3d.startPoint exampleArc
    --> Point3d ( 2, 0, 1 )

-}
startPoint : Arc3d -> Point3d
startPoint (Arc3d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc3d.endPoint exampleArc
    --> Point3d ( 0, 2, 1 )

-}
endPoint : Arc3d -> Point3d
endPoint arc =
    Point3d.rotateAround (axis arc) (sweptAngle arc) (startPoint arc)


{-| Get the point along an arc at a given parameter value. A parameter value of
0 corresponds to the start point of the arc and a value of 1 corresponds to the
end point.

    Arc3d.point exampleArc 0.5
    --> Point3d ( 1.4142, 1.4142, 1 )

-}
point : Arc3d -> Float -> Point3d
point arc parameter =
    let
        angle =
            parameter * sweptAngle arc
    in
        Point3d.rotateAround (axis arc) angle (startPoint arc)


{-| Get the swept angle of an arc in radians.

    Arc2d.sweptAngle exampleArc
    --> 1.5708

A positive swept angle means that the arc is formed by rotating the given start
point counterclockwise around the central axis, and vice versa for a negative
angle.

-}
sweptAngle : Arc3d -> Float
sweptAngle (Arc3d properties) =
    properties.sweptAngle


{-| Scale an arc about the given center point by the given scale.

    point =
        Point3d ( 1, 0, 1 )

    Arc3d.scaleAbout point 2 exampleArc
    --> Arc3d
    -->     { axis =
    -->         Axis3d
    -->             { originPoint = Point3d ( -1, 0, -1 )
    -->             , direction = Direction3d.z
    -->             }
    -->     , startPoint = Point3d ( 3, 0, 1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
scaleAbout : Point3d -> Float -> Arc3d -> Arc3d
scaleAbout point scale arc =
    let
        scalePoint =
            Point3d.scaleAbout point scale

        currentAxis =
            axis arc

        scaledOrigin =
            scalePoint (Axis3d.originPoint currentAxis)

        currentAxisDirection =
            Axis3d.direction currentAxis

        scaledDirection =
            if scale < 0.0 then
                Direction3d.flip currentAxisDirection
            else
                currentAxisDirection

        scaledAxis =
            Axis3d { originPoint = scaledOrigin, direction = scaledDirection }
    in
        Arc3d
            { axis = scaledAxis
            , startPoint = scalePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


{-| Rotate an arc around a given axis by a given angle (in radians).

    Arc3d.rotateAround Axis3d.x (degrees 90) exampleArc
    --> Arc3d
    -->     { axis = Axis3d.flip Axis3d.y
    -->     , startPoint = Point3d ( 2, -1, 0 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
rotateAround : Axis3d -> Float -> Arc3d -> Arc3d
rotateAround rotationAxis angle =
    let
        rotateAxis =
            Axis3d.rotateAround rotationAxis angle

        rotatePoint =
            Point3d.rotateAround rotationAxis angle
    in
        \arc ->
            Arc3d
                { axis = rotateAxis (axis arc)
                , startPoint = rotatePoint (startPoint arc)
                , sweptAngle = sweptAngle arc
                }


{-| Translate an arc by a given displacement.

    displacement =
        Vector3d ( 2, 1, 3 )

    Arc3d.translateBy displacement exampleArc
    --> Arc3d
    -->     { axis =
    -->         Axis3d
    -->             { originPoint = Point3d ( 2, 1, 3 )
    -->             , direction = Direction3d.z
    -->             }
    -->     , startPoint = Point3d ( 4, 1, 4 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector3d -> Arc3d -> Arc3d
translateBy displacement arc =
    Arc3d
        { axis = Axis3d.translateBy displacement (axis arc)
        , startPoint = Point3d.translateBy displacement (startPoint arc)
        , sweptAngle = sweptAngle arc
        }


{-| Mirror an arc across a given plane.

    Arc3d.mirrorAcross Plane3d.xy exampleArc
    Arc3d
        { axis = Axis3d.flip Axis3d.z
        , startPoint = Point3d ( 2, 0, -1 )
        , sweptAngle = degrees -90
        }

Note that this flips the sign of the arc's swept angle.

-}
mirrorAcross : Plane3d -> Arc3d -> Arc3d
mirrorAcross plane =
    let
        mirrorAxis =
            Axis3d.mirrorAcross plane

        mirrorPoint =
            Point3d.mirrorAcross plane
    in
        \arc ->
            Arc3d
                { axis = mirrorAxis (axis arc)
                , startPoint = mirrorPoint (startPoint arc)
                , sweptAngle = -(sweptAngle arc)
                }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Arc3d.relativeTo localFrame exampleArc
    --> Arc3d
    -->     { axis =
    -->         Axis3d
    -->             { originPoint = Point3d ( -1, -2, -3 )
    -->             , direction = Direction3d.z
    -->             }
    -->     , startPoint = Point3d ( 1, -2, -2 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame3d -> Arc3d -> Arc3d
relativeTo frame arc =
    Arc3d
        { axis = Axis3d.relativeTo frame (axis arc)
        , startPoint = Point3d.relativeTo frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                (sweptAngle arc)
            else
                -(sweptAngle arc)
        }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Arc3d.placeIn localFrame exampleArc
    --> Arc3d
    -->     { axis =
    -->         Axis3d
    -->             { originPoint = Point3d ( 1, 2, 3 )
    -->             , direction = Direction3d.z
    -->             }
    -->     , startPoint = Point3d ( 3, 2, 4 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame3d -> Arc3d -> Arc3d
placeIn frame arc =
    Arc3d
        { axis = Axis3d.placeIn frame (axis arc)
        , startPoint = Point3d.placeIn frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                (sweptAngle arc)
            else
                -(sweptAngle arc)
        }
