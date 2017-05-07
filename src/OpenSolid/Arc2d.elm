module OpenSolid.Arc2d
    exposing
        ( Length
        , WindingDirection
        , short
        , long
        , clockwise
        , counterclockwise
        , throughPoints
        , fromEndpoints
        , centerPoint
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
        , placeOnto
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/arc2d.svg" alt="Arc2d" width="160">

An `Arc2d` is a section of a circle, defined by its center point, start
point and swept angle (the counterclockwise angle from the start point to the
end point). This module includes functionality for

  - Constructing arcs through given points and/or with a given radius
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems
  - Placing 2D arcs onto sketch planes to result in 3D arcs

Arcs can be constructed explicitly by passing a record with `centerPoint`,
`startPoint` and `sweptAngle` fields to the `Arc2d` constructor, for example

    exampleArc =
        Arc2d
            { centerPoint = Point2d ( 1, 1 )
            , startPoint = Point2d ( 3, 1 )
            , sweptAngle = degrees 90
            }


# Constructors

@docs Length, WindingDirection, short, long, clockwise, counterclockwise, throughPoints, fromEndpoints


# Accessors

@docs centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs point


# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Circle2d as Circle2d


{-| Argument type used in [`fromEndpoints`](#fromEndpoints).
-}
type Length
    = Short
    | Long


{-| Argument type used in [`fromEndpoints`](#fromEndpoints).
-}
type WindingDirection
    = Clockwise
    | Counterclockwise


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
short : Length
short =
    Short


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
long : Length
long =
    Long


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
clockwise : WindingDirection
clockwise =
    Clockwise


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
counterclockwise : WindingDirection
counterclockwise =
    Counterclockwise


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    Arc2d.throughPoints
        Point2d.origin
        (Point2d ( 1, 0 ))
        (Point2d ( 0, 1 ))
    --> Just
    -->     (Arc2d
    -->         { centerPoint = Point2d ( 0.5, 0.5 )
    -->         , startPoint = Point2d.origin
    -->         , sweptAngle = degrees 270
    -->         }
    -->     )

    Arc2d.throughPoints
        (Point2d ( 1, 0 ))
        Point2d.origin
        (Point2d ( 0, 1 ))
    --> Just
    -->     (Arc2d
    -->         { centerPoint = Point2d ( 0.5, 0.5 )
    -->         , startPoint = Point2d ( 1, 0 )
    -->         , sweptAngle = degrees -180
    -->         }
    -->     )

    Arc2d.throughPoints
        Point2d.origin
        (Point2d ( 1, 0 ))
        (Point2d ( 2, 0 ))
    --> Nothing

    Arc2d.throughPoints
        Point2d.origin
        Point2d.origin
        (Point2d ( 1, 0 ))
    --> Nothing

-}
throughPoints : Point2d -> Point2d -> Point2d -> Maybe Arc2d
throughPoints firstPoint secondPoint thirdPoint =
    Circle2d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\circle ->
                let
                    centerPoint =
                        Circle2d.centerPoint circle

                    firstVector =
                        Point2d.vectorFrom centerPoint firstPoint

                    secondVector =
                        Point2d.vectorFrom centerPoint secondPoint

                    thirdVector =
                        Point2d.vectorFrom centerPoint thirdPoint
                in
                    Maybe.map3
                        (\firstDirection secondDirection thirdDirection ->
                            let
                                partial =
                                    Direction2d.angleFrom firstDirection
                                        secondDirection

                                full =
                                    Direction2d.angleFrom firstDirection
                                        thirdDirection

                                sweptAngle =
                                    if partial >= 0 && full >= partial then
                                        full
                                    else if partial <= 0 && full <= partial then
                                        full
                                    else if full >= 0 then
                                        full - 2 * pi
                                    else
                                        full + 2 * pi
                            in
                                Arc2d
                                    { centerPoint = centerPoint
                                    , startPoint = firstPoint
                                    , sweptAngle = sweptAngle
                                    }
                        )
                        (Vector2d.direction firstVector)
                        (Vector2d.direction secondVector)
                        (Vector2d.direction thirdVector)
            )


{-| Attempt to construct an arc with the given start point, end point and
radius. For any given valid set of start point, end point and radius, there are
four possible results, so two more arguments are required to fully specify the
arc to create:

  - For the fourth argument, pass either [`Arc2d.short`](#short) or
    [`Arc2d.long`](#long) to indicate whether the returned arc should be have a
    swept angle less than or greater than 180 degrees respectively.
  - For the fifth argument, pass either [`Arc2d.counterclockwise`](#counterclockwise)
    or [`Arc2d.clockwise`](#clockwise) to indicate whether the returned arc
    should be counterclockwise (have a positive swept angle) or clockwise (have
    a negative swept angle).

For example:

    p1 =
        Point2d ( 1, 0 )

    p2 =
        Point2d ( 0, 1 )

    Arc2d.fromEndpoints p1 p2 1 Arc2d.short Arc2d.counterclockwise
    --> Just
    -->     (Arc2d
    -->         { startPoint = Point2d ( 1, 0 )
    -->         , centerPoint = Point2d.origin
    -->         , sweptAngle = degrees 90
    -->         }
    -->     )

    Arc2d.fromEndpoints p1 p2 1 Arc2d.short Arc2d.clockwise
    --> Just
    -->     (Arc2d
    -->         { startPoint = Point2d ( 1, 0 )
    -->         , centerPoint = Point2d ( 1, 1 )
    -->         , sweptAngle = degrees -90
    -->         }
    -->     )

    Arc2d.fromEndpoints p1 p2 1 Arc2d.long Arc2d.counterclockwise
    --> Just
    -->     (Arc2d
    -->         { startPoint = Point2d ( 1, 0 )
    -->         , centerPoint = Point2d ( 1, 1 )
    -->         , sweptAngle = degrees 270
    -->         }
    -->     )

    Arc2d.fromEndpoints p1 p2 1 Arc2d.long Arc2d.clockwise
    --> Just
    -->     (Arc2d
    -->         { startPoint = Point2d ( 1, 0 )
    -->         , centerPoint = Point2d.origin
    -->         , sweptAngle = degrees -270
    -->         }
    -->     )

    Arc2d.fromEndpoints p1 p2 2 Arc2d.short Arc2d.counterclockwise
    --> Just
    -->     (Arc2d
    -->         { startPoint = Point2d ( 1, 0 )
    -->         , centerPoint = Point2d ( -0.8229, -0.8229 )
    -->         , sweptAngle = degrees 41.4096
    -->         }
    -->     )

If the start and end points are coincident or the distance between them is more
than twice the given radius, returns `Nothing`:

    Arc2d.fromEndpoints p1 p2 0.5 Arc2d.short Arc2d.counterclockwise
    --> Nothing

Note that this means it is dangerous to use this function to construct 180
degree arcs (half circles), since in this case due to numerical roundoff the
distance between the two given points may appear to be slightly more than twice
the given radius. In this case it is safer to use a more specialized approach,
such as

    halfCircle =
        Arc2d
            { startPoint = firstPoint
            , centerPoint = Point2d.midpoint firstPoint secondPoint
            , sweptAngle = degrees 180 -- or 'degrees -180' for a clockwise arc
            }

-}
fromEndpoints : Point2d -> Point2d -> Float -> Length -> WindingDirection -> Maybe Arc2d
fromEndpoints startPoint endPoint radius lengthType windingDirection =
    let
        chord =
            LineSegment2d ( startPoint, endPoint )

        squaredRadius =
            radius * radius

        squaredHalfLength =
            LineSegment2d.squaredLength chord / 4
    in
        if squaredRadius >= squaredHalfLength then
            LineSegment2d.normalDirection chord
                |> Maybe.map
                    (\offsetDirection ->
                        let
                            offsetMagnitude =
                                sqrt (squaredRadius - squaredHalfLength)

                            offsetDistance =
                                case ( windingDirection, lengthType ) of
                                    ( Counterclockwise, Short ) ->
                                        offsetMagnitude

                                    ( Clockwise, Long ) ->
                                        offsetMagnitude

                                    ( Clockwise, Short ) ->
                                        -offsetMagnitude

                                    ( Counterclockwise, Long ) ->
                                        -offsetMagnitude

                            offset =
                                Vector2d.in_ offsetDirection offsetDistance

                            midpoint =
                                LineSegment2d.midpoint chord

                            centerPoint =
                                Point2d.translateBy offset midpoint

                            halfLength =
                                sqrt squaredHalfLength

                            shortAngle =
                                2 * asin (halfLength / radius)

                            sweptAngle =
                                case ( windingDirection, lengthType ) of
                                    ( Counterclockwise, Short ) ->
                                        shortAngle

                                    ( Clockwise, Short ) ->
                                        -shortAngle

                                    ( Counterclockwise, Long ) ->
                                        2 * pi - shortAngle

                                    ( Clockwise, Long ) ->
                                        shortAngle - 2 * pi
                        in
                            Arc2d
                                { centerPoint = centerPoint
                                , startPoint = startPoint
                                , sweptAngle = sweptAngle
                                }
                    )
        else
            Nothing


{-| Get the center point of an arc.

    Arc2d.centerPoint exampleArc
    --> Point2d ( 1, 1 )

-}
centerPoint : Arc2d -> Point2d
centerPoint (Arc2d properties) =
    properties.centerPoint


{-| Get the radius of an arc.

    Arc2d.radius exampleArc
    --> 2

-}
radius : Arc2d -> Float
radius arc =
    Point2d.distanceFrom (centerPoint arc) (startPoint arc)


{-| Get the start point of an arc.

    Arc2d.startPoint exampleArc
    --> Point2d ( 3, 1 )

-}
startPoint : Arc2d -> Point2d
startPoint (Arc2d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc2d.endPoint exampleArc
    --> Point2d ( 1, 3 )

-}
endPoint : Arc2d -> Point2d
endPoint arc =
    Point2d.rotateAround (centerPoint arc) (sweptAngle arc) (startPoint arc)


{-| Get the swept angle of an arc in radians.

    Arc2d.sweptAngle exampleArc
    --> 1.5708

The result will be positive for a counterclockwise arc and negative for a
clockwise one.

-}
sweptAngle : Arc2d -> Float
sweptAngle (Arc2d properties) =
    properties.sweptAngle


{-| Get the point along an arc at a given parameter value. A parameter value of
0 corresponds to the start point of the arc and a value of 1 corresponds to the
end point.

    Arc2d.point exampleArc 0.5
    --> Point2d ( 2.4142, 2.4142 )

-}
point : Arc2d -> Float -> Point2d
point arc parameter =
    let
        angle =
            parameter * sweptAngle arc
    in
        Point2d.rotateAround (centerPoint arc) angle (startPoint arc)


{-| Scale an arc about a given point by a given scale.

    Arc2d.scaleAbout (Point2d ( 0, 1 )) 2 exampleArc
    --> Arc2d
    -->     { startPoint = Point2d ( 6, 1 )
    -->     , centerPoint = Point2d ( 2, 1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
scaleAbout : Point2d -> Float -> Arc2d -> Arc2d
scaleAbout point scale arc =
    let
        scalePoint =
            Point2d.scaleAbout point scale
    in
        Arc2d
            { centerPoint = scalePoint (centerPoint arc)
            , startPoint = scalePoint (startPoint arc)
            , sweptAngle =
                if scale > 0 then
                    sweptAngle arc
                else
                    -(sweptAngle arc)
            }


{-| Rotate an arc around a given point by a given angle.

    Arc2d.rotateAround Point2d.origin (degrees 90)
    Arc2d
        { startPoint = Point2d ( -1, 3 )
        , centerPoint = Point2d ( -1, 1 )
        , sweptAngle = degrees 90
        }

-}
rotateAround : Point2d -> Float -> Arc2d -> Arc2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
        \arc ->
            Arc2d
                { centerPoint = rotatePoint (centerPoint arc)
                , startPoint = rotatePoint (startPoint arc)
                , sweptAngle = sweptAngle arc
                }


{-| Translate an arc by a given displacement.

    displacement =
        Vector2d ( 2, 3 )

    Arc2d.translateBy displacement exampleArc
    --> Arc2d
    -->     { startPoint = Point2d ( 5, 4 )
    -->     , centerPoint = Point2d ( 3, 4 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector2d -> Arc2d -> Arc2d
translateBy displacement arc =
    let
        translatePoint =
            Point2d.translateBy displacement
    in
        Arc2d
            { centerPoint = translatePoint (centerPoint arc)
            , startPoint = translatePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


{-| Mirror an arc across a given axis.

    Arc2d.mirrorAcross Axis2d.y exampleArc
    --> Arc2d
    -->     { startPoint = Point2d ( -3, 1 )
    -->     , centerPoint = Point2d ( -1, 1 )
    -->     , sweptAngle = degrees -90
    -->     }

-}
mirrorAcross : Axis2d -> Arc2d -> Arc2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis
    in
        \arc ->
            Arc2d
                { centerPoint = mirrorPoint (centerPoint arc)
                , startPoint = mirrorPoint (startPoint arc)
                , sweptAngle = -(sweptAngle arc)
                }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Arc2d.relativeTo localFrame exampleArc
    --> Arc2d
    -->     { startPoint = Point2d ( 2, -1 )
    -->     , centerPoint = Point2d ( 0, -1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame2d -> Arc2d -> Arc2d
relativeTo frame arc =
    let
        relativePoint =
            Point2d.relativeTo frame
    in
        Arc2d
            { centerPoint = relativePoint (centerPoint arc)
            , startPoint = relativePoint (startPoint arc)
            , sweptAngle =
                if Frame2d.isRightHanded frame then
                    (sweptAngle arc)
                else
                    -(sweptAngle arc)
            }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Arc2d.placeIn localFrame exampleArc
    --> Arc2d
    -->     { startPoint = Point2d ( 4, 3 )
    -->     , centerPoint = Point2d ( 2, 3 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame2d -> Arc2d -> Arc2d
placeIn frame arc =
    let
        placePoint =
            Point2d.placeIn frame
    in
        Arc2d
            { centerPoint = placePoint (centerPoint arc)
            , startPoint = placePoint (startPoint arc)
            , sweptAngle =
                if Frame2d.isRightHanded frame then
                    (sweptAngle arc)
                else
                    -(sweptAngle arc)
            }


{-| Take an arc defined in 2D coordinates within a particular sketch plane and
return the corresponding arc in 3D.

    Arc2d.placeOnto SketchPlane3d.yz exampleArc
    --> Arc3d
    -->     { startPoint = Point3d ( 0, 3, 1 )
    -->     , axis =
    -->         Axis3d
    -->             { originPoint = Point3d ( 0, 1, 1 )
    -->             , direction = Direction3d.x
    -->             }
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeOnto : SketchPlane3d -> Arc2d -> Arc3d
placeOnto sketchPlane arc =
    let
        place =
            Point2d.placeOnto sketchPlane

        axis =
            Axis3d
                { originPoint = place (centerPoint arc)
                , direction = SketchPlane3d.normalDirection sketchPlane
                }
    in
        Arc3d
            { axis = axis
            , startPoint = place (startPoint arc)
            , sweptAngle = sweptAngle arc
            }
