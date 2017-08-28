module OpenSolid.Arc3d
    exposing
        ( Arc3d
        , around
        , axis
        , centerPoint
        , endPoint
        , evaluate
        , mirrorAcross
        , on
        , placeIn
        , pointOn
        , radius
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , startPoint
        , sweptAngle
        , through
        , toPolyline
        , translateBy
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/arc3d.svg" alt="Arc3d" width="160">

An `Arc3d` is a section of a circle in 3D, defined by its central axis,
start point and swept angle (the counterclockwise angle around the axis from the
start point to the arc's end point). This module includes functionality for

  - Constructing arcs through given points
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc3d


# Constructors

@docs around, on, through


# Properties

@docs axis, centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs pointOn, evaluate


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| -}
type alias Arc3d =
    Internal.Arc3d


{-| Construct an arc around the given axis, with the given start point and swept
angle:

    exampleArc =
        Arc3d.around Axis3d.z
            { startPoint =
                Point3d.withCoordinates ( 1, 1, 0 )
            , sweptAngle = degrees 90
            }

    Arc3d.centerPoint exampleArc
    --> Point3d.origin

    Arc3d.endPoint exampleArc
    --> Point3d.withCoordinates ( -1, 1, 0 )

Positive swept angles result in a counterclockwise (right-handed) rotation
around the given axis and vice versa for negative swept angles.

-}
around : Axis3d -> { startPoint : Point3d, sweptAngle : Float } -> Arc3d
around axis { startPoint, sweptAngle } =
    Internal.Arc3d
        { axis =
            Axis3d.with
                { originPoint = Point3d.projectOntoAxis axis startPoint
                , direction = Axis3d.direction axis
                }
        , startPoint = startPoint
        , sweptAngle = sweptAngle
        }


{-| Construct a 3D arc lying _on_ a sketch plane by providing a 2D arc specified
in XY coordinates _within_ the sketch plane.

    arc =
        Arc3d.on SketchPlane3d.xz <|
            Arc2d.with
                { centerPoint =
                    Point2d.withCoordinates ( 1, 1 )
                , startPoint =
                    Point2d.withCoordinates ( 3, 1 )
                , sweptAngle = degrees 90
                }

    Arc3d.centerPoint arc
    --> Point3d.withCoordinates ( 1, 0, 1 )

    Arc3d.radius arc
    --> 2

    Arc3d.startPoint arc
    --> Point3d.withCoordinates ( 3, 0, 1 )

    Arc3d.endPoint arc
    --> Point3d.withCoordinates ( 1, 0, 3 )

-}
on : SketchPlane3d -> Arc2d -> Arc3d
on sketchPlane arc2d =
    let
        place =
            Point3d.on sketchPlane

        axis =
            Axis3d.with
                { originPoint = place (Arc2d.centerPoint arc2d)
                , direction = SketchPlane3d.normalDirection sketchPlane
                }
    in
    around axis
        { startPoint = place (Arc2d.startPoint arc2d)
        , sweptAngle = Arc2d.sweptAngle arc2d
        }


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    p1 =
        Point3d.withCoordinates ( 0, 0, 1 )

    p2 =
        Point3d.origin

    p3 =
        Point3d.withCoordinates ( 0, 1, 0 )

    Arc3d.through ( p1, p2, p3 )
    --> Just
    -->     (Arc3d.on SketchPlane3d.yz <|
    -->         Arc2d.with
    -->             { centerPoint =
    -->                 Point2d.withCoordinates
    -->                     ( 0.5, 0.5 )
    -->             , startPoint =
    -->                 Point3d.withCoordinates ( 0, 1 )
    -->             , sweptAngle = degrees 180
    -->             }
    -->     )

-}
through : ( Point3d, Point3d, Point3d ) -> Maybe Arc3d
through points =
    SketchPlane3d.through points
        |> Maybe.andThen
            (\sketchPlane ->
                let
                    ( firstPoint, secondPoint, thirdPoint ) =
                        points
                in
                Arc2d.through
                    ( Point3d.projectInto sketchPlane firstPoint
                    , Point3d.projectInto sketchPlane secondPoint
                    , Point3d.projectInto sketchPlane thirdPoint
                    )
                    |> Maybe.map (on sketchPlane)
            )


{-| Get the central axis of an arc. The origin point of the axis will be equal
to the center point of the arc.

    Arc3d.axis exampleArc
    --> Axis3d.z

-}
axis : Arc3d -> Axis3d
axis (Internal.Arc3d properties) =
    properties.axis


{-| Get the center point of an arc.

    Arc3d.centerPoint exampleArc
    --> Point3d.origin

-}
centerPoint : Arc3d -> Point3d
centerPoint arc =
    Axis3d.originPoint (axis arc)


{-| Get the radius of an arc.

    Arc3d.radius exampleArc
    --> 1.4142

-}
radius : Arc3d -> Float
radius arc =
    Point3d.distanceFrom (centerPoint arc) (startPoint arc)


{-| Get the start point of an arc.

    Arc3d.startPoint exampleArc
    --> Point3d.withCoordinates ( 1, 1, 0 )

-}
startPoint : Arc3d -> Point3d
startPoint (Internal.Arc3d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc3d.endPoint exampleArc
    --> Point3d.withCoordinates ( -1, 1, 0 )

-}
endPoint : Arc3d -> Point3d
endPoint arc =
    Point3d.rotateAround (axis arc) (sweptAngle arc) (startPoint arc)


{-| Get the point along an arc at a given parameter value. A parameter value of
0 corresponds to the start point of the arc and a value of 1 corresponds to the
end point.

    Arc3d.pointOn exampleArc 0.5
    --> Point3d.withCoordinates ( 0, 1.4142, 0 )

-}
pointOn : Arc3d -> Float -> Point3d
pointOn arc =
    let
        arcCenterPoint =
            centerPoint arc

        axialVector =
            Direction3d.toVector (Axis3d.direction (axis arc))

        xVector =
            Vector3d.from arcCenterPoint (startPoint arc)

        yVector =
            Vector3d.crossProduct axialVector xVector

        ( x0, y0, z0 ) =
            Point3d.coordinates arcCenterPoint

        ( x1, y1, z1 ) =
            Vector3d.components xVector

        ( x2, y2, z2 ) =
            Vector3d.components yVector

        arcSweptAngle =
            sweptAngle arc
    in
    \t ->
        let
            angle =
                t * arcSweptAngle

            cosAngle =
                cos angle

            sinAngle =
                sin angle
        in
        Point3d.withCoordinates
            ( x0 + x1 * cosAngle + x2 * sinAngle
            , y0 + y1 * cosAngle + y2 * sinAngle
            , z0 + z1 * cosAngle + z2 * sinAngle
            )


{-| Evaluate an arc at a given parameter value, returning the point on the arc
at that parameter value and the derivative with respect to that parameter value.

    Arc3d.evaluate exampleArc 0
    --> ( Point3d.withCoordinates ( 1, 1, 0 )
    --> , Vector3d.withComponents ( -1.5708, 1.5708, 0 )
    --> )

    Arc3d.evaluate exampleArc 0.5
    --> ( Point3d.withCoordinates ( 1.4142, 0, 0 )
    --> , Vector3d.withComponents ( -2.2214, 0, 0 )
    --> )

    Arc3d.evaluate exampleArc 1
    --> ( Point3d.withCoordinates ( -1, 1, 0 )
    --> , Vector3d.withComponents ( -1.5708, -1.5708, 0 )
    --> )

-}
evaluate : Arc3d -> Float -> ( Point3d, Vector3d )
evaluate arc =
    let
        arcCenterPoint =
            centerPoint arc

        axialVector =
            Direction3d.toVector (Axis3d.direction (axis arc))

        xVector =
            Vector3d.from arcCenterPoint (startPoint arc)

        yVector =
            Vector3d.crossProduct axialVector xVector

        ( x0, y0, z0 ) =
            Point3d.coordinates arcCenterPoint

        ( x1, y1, z1 ) =
            Vector3d.components xVector

        ( x2, y2, z2 ) =
            Vector3d.components yVector

        arcSweptAngle =
            sweptAngle arc
    in
    \t ->
        let
            angle =
                t * arcSweptAngle

            cosAngle =
                cos angle

            sinAngle =
                sin angle
        in
        ( Point3d.withCoordinates
            ( x0 + x1 * cosAngle + x2 * sinAngle
            , y0 + y1 * cosAngle + y2 * sinAngle
            , z0 + z1 * cosAngle + z2 * sinAngle
            )
        , Vector3d.withComponents
            ( arcSweptAngle * (cosAngle * x2 - sinAngle * x1)
            , arcSweptAngle * (cosAngle * y2 - sinAngle * y1)
            , arcSweptAngle * (cosAngle * z2 - sinAngle * z1)
            )
        )


numApproximationSegments : Float -> Arc3d -> Int
numApproximationSegments tolerance arc =
    if 0 < tolerance && tolerance < radius arc then
        let
            maxSegmentAngle =
                sqrt (8 * tolerance / radius arc)
        in
        ceiling (abs (sweptAngle arc) / maxSegmentAngle)
    else
        1


{-| Approximate an arc as a polyline, within the specified tolerance.

    Arc3d.toPolyline 0.1 exampleArc
    --> Polyline3d.withVertices
    -->     [ Point3d.withCoordinates ( 1, 1, 0 )
    -->     , Point3d.withCoordinates ( 0.366, 1.366, 0 )
    -->     , Point3d.withCoordinates ( -0.366, 1.366, 0 )
    -->     , Point3d.withCoordinates ( -1, 1, 0 )
    -->     ]

A tolerance of zero will be treated as infinity (a single line segment will be
returned).

-}
toPolyline : Float -> Arc3d -> Polyline3d
toPolyline tolerance arc =
    let
        numSegments =
            numApproximationSegments tolerance arc

        point index =
            pointOn arc (toFloat index / toFloat numSegments)

        points =
            List.range 0 numSegments |> List.map point
    in
    Polyline3d.withVertices points


{-| Get the swept angle of an arc in radians.

    Arc3d.sweptAngle exampleArc
    --> 1.5708

A positive swept angle means that the arc is formed by rotating the given start
point counterclockwise around the central axis, and vice versa for a negative
angle.

-}
sweptAngle : Arc3d -> Float
sweptAngle (Internal.Arc3d properties) =
    properties.sweptAngle


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa. The resulting arc will have the same axis as the original
but a swept angle with the opposite sign.

    Arc3d.reverse exampleArc
    --> Arc3d.around Axis3d.z
    -->     { startPoint =
    -->         Point3d.withCoordinates ( -1, 1, 0 )
    -->     , sweptAngle = degrees -90
    -->     }

-}
reverse : Arc3d -> Arc3d
reverse arc =
    around (axis arc)
        { startPoint = endPoint arc
        , sweptAngle = -(sweptAngle arc)
        }


{-| Scale an arc about the given center point by the given scale.

    point =
        Point3d.withCoordinates ( 0, -1, 0 )

    Arc3d.scaleAbout point 2 exampleArc
    --> Arc3d.around
    -->     (Axis3d.with
    -->         { originPoint =
    -->             Point3d.withCoordinates ( 0, 1, 0 )
    -->         , direction = Direction3d.z
    -->         }
    -->     )
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 2, 3, 0 )
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
            Axis3d.with
                { originPoint = scaledOrigin
                , direction = scaledDirection
                }
    in
    around scaledAxis
        { startPoint = scalePoint (startPoint arc)
        , sweptAngle = sweptAngle arc
        }


{-| Rotate an arc around a given axis by a given angle (in radians).

    Arc3d.rotateAround Axis3d.x (degrees 90) exampleArc
    --> Arc3d.around (Axis3d.flip Axis3d.y)
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 1, 0, 1 )
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
        around (rotateAxis (axis arc))
            { startPoint = rotatePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


{-| Translate an arc by a given displacement.

    displacement =
        Vector3d.withComponents ( 2, 1, 3 )

    Arc3d.translateBy displacement exampleArc
    --> Arc3d.around
    -->     (Axis3d.with
    -->         { originPoint = Point3d ( 2, 1, 3 )
    -->         , direction = Direction3d.z
    -->         }
    -->     )
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 3, 2, 3 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector3d -> Arc3d -> Arc3d
translateBy displacement arc =
    around (Axis3d.translateBy displacement (axis arc))
        { startPoint = Point3d.translateBy displacement (startPoint arc)
        , sweptAngle = sweptAngle arc
        }


{-| Mirror an arc across a given plane.

    Arc3d.mirrorAcross Plane3d.xy exampleArc
    --> Arc3d.around (Axis3d.flip Axis3d.z)
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 1, 1, 0 )
    -->     , sweptAngle = degrees -90
    -->     }

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
        around (mirrorAxis (axis arc))
            { startPoint = mirrorPoint (startPoint arc)
            , sweptAngle = -(sweptAngle arc)
            }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    Arc3d.relativeTo localFrame exampleArc
    --> Arc3d.around
    -->     (Axis3d.with
    -->         { originPoint = Point3d ( -1, -2, -3 )
    -->         , direction = Direction3d.z
    -->         }
    -->     )
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 0, -1, -3 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame3d -> Arc3d -> Arc3d
relativeTo frame arc =
    around (Axis3d.relativeTo frame (axis arc))
        { startPoint = Point3d.relativeTo frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                sweptAngle arc
            else
                -(sweptAngle arc)
        }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d.withCoordinates ( 1, 2, 3 ))

    Arc3d.placeIn localFrame exampleArc
    --> Arc3d.around
    -->     (Axis3d.with
    -->         { originPoint =
    -->             Point3d.withCoordinates ( 1, 2, 3 )
    -->         , direction = Direction3d.z
    -->         }
    -->     )
    -->     { startPoint =
    -->         Point3d.withCoordinates ( 2, 3, 3 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame3d -> Arc3d -> Arc3d
placeIn frame arc =
    around (Axis3d.placeIn frame (axis arc))
        { startPoint = Point3d.placeIn frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                sweptAngle arc
            else
                -(sweptAngle arc)
        }
