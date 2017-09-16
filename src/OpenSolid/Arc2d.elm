module OpenSolid.Arc2d
    exposing
        ( Arc2d
        , SweptAngle
        , centerPoint
        , derivative
        , endPoint
        , evaluate
        , fromEndpoints
        , largeNegative
        , largePositive
        , mirrorAcross
        , placeIn
        , pointOn
        , radius
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , smallNegative
        , smallPositive
        , startPoint
        , sweptAngle
        , throughPoints
        , toPolyline
        , translateBy
        , with
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/arc2d.svg" alt="Arc2d" width="160">

An `Arc2d` is a section of a circle, defined by its center point, start
point and swept angle (the counterclockwise angle from the start point to the
end point). This module includes functionality for

  - Constructing arcs through given points and/or with a given radius
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc2d


# Constructors

@docs with, throughPoints, fromEndpoints, SweptAngle, smallPositive, smallNegative, largePositive, largeNegative


# Properties

@docs centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs pointOn, derivative, evaluate


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias Arc2d =
    Internal.Arc2d


{-| Construct an arc from its center point, start point, and swept angle:

    exampleArc =
        Arc2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , startPoint =
                Point2d.fromCoordinates ( 3, 1 )
            , sweptAngle = degrees 90
            }

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

A positive swept angle means that the arc is formed by rotating the start point
counterclockwise around the center point. A negative swept angle results in
a clockwise arc instead.

-}
with : { centerPoint : Point2d, startPoint : Point2d, sweptAngle : Float } -> Arc2d
with =
    Internal.Arc2d


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    Arc2d.throughPoints
        ( Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        , Point2d.fromCoordinates ( 0, 1 )
        )
    --> Just
    -->     (Arc2d.with
    -->         { centerPoint =
    -->             Point2d.fromCoordinates ( 0.5, 0.5 )
    -->         , startPoint = Point2d.origin
    -->         , sweptAngle = degrees 270
    -->         }
    -->     )

    Arc2d.throughPoints
        ( Point2d.fromCoordinates ( 1, 0 )
        , Point2d.origin
        , Point2d.fromCoordinates ( 0, 1 )
        )
    --> Just
    -->     (Arc2d.with
    -->         { centerPoint =
    -->             Point2d.fromCoordinates ( 0.5, 0.5 )
    -->         , startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , sweptAngle = degrees -180
    -->         }
    -->     )

    Arc2d.throughPoints
        ( Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        , Point2d.fromCoordinates ( 2, 0 )
        )
    --> Nothing

    Arc2d.throughPoints
        ( Point2d.origin
        , Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        )
    --> Nothing

-}
throughPoints : ( Point2d, Point2d, Point2d ) -> Maybe Arc2d
throughPoints points =
    Point2d.circumcenter points
        |> Maybe.andThen
            (\centerPoint ->
                let
                    ( firstPoint, secondPoint, thirdPoint ) =
                        points

                    firstVector =
                        Vector2d.from centerPoint firstPoint

                    secondVector =
                        Vector2d.from centerPoint secondPoint

                    thirdVector =
                        Vector2d.from centerPoint thirdPoint
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
                        with
                            { centerPoint = centerPoint
                            , startPoint = firstPoint
                            , sweptAngle = sweptAngle
                            }
                    )
                    (Vector2d.direction firstVector)
                    (Vector2d.direction secondVector)
                    (Vector2d.direction thirdVector)
            )


{-| Argument type used in [`fromEndpoints`](#fromEndpoints).
-}
type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallPositive : SweptAngle
smallPositive =
    SmallPositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallNegative : SweptAngle
smallNegative =
    SmallNegative


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largePositive : SweptAngle
largePositive =
    LargePositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largeNegative : SweptAngle
largeNegative =
    LargeNegative


{-| Attempt to construct an arc with the given start point, end point and
radius. For any given valid set of start point, end point and radius, there are
four possible results, so the `sweptAngle` argument is used to specify which
arc to create:

  - `Arc2d.smallPositive` will result in a counterclockwise arc with a small
    swept angle (in the range 0..180 degrees)
  - `Arc2d.smallNegative` will result in a clockwise arc with a small swept
    angle (in the range -180..0 degrees)
  - `Arc2d.largePositive` will result in a counterclockwise arc with a large
    swept angle (in the range 180..360 degrees)
  - `Arc2d.largeNegative` will result in a clockwise arc with a large swept
    angle (in the range -360..-180 degrees)

For example:

    p1 =
        Point2d.fromCoordinates ( 1, 0 )

    p2 =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 1
        , sweptAngle = Arc2d.smallPositive
        }
    --> Just
    -->     (Arc2d.with
    -->         { startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , centerPoint = Point2d.origin
    -->         , sweptAngle = degrees 90
    -->         }
    -->     )

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 1
        , sweptAngle = Arc2d.smallNegative
        }
    --> Just
    -->     (Arc2d.with
    -->         { startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , centerPoint =
    -->             Point2d.fromCoordinates ( 1, 1 )
    -->         , sweptAngle = degrees -90
    -->         }
    -->     )

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 1
        , sweptAngle = Arc2d.largePositive
        }
    --> Just
    -->     (Arc2d.with
    -->         { startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , centerPoint =
    -->             Point2d.fromCoordinates ( 1, 1 )
    -->         , sweptAngle = degrees 270
    -->         }
    -->     )

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 1
        , sweptAngle = Arc2d.largeNegative
        }
    --> Just
    -->     (Arc2d.with
    -->         { startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , centerPoint = Point2d.origin
    -->         , sweptAngle = degrees -270
    -->         }
    -->     )

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 2
        , sweptAngle = Arc2d.smallPositive
        }
    --> Just
    -->     (Arc2d.with
    -->         { startPoint =
    -->             Point2d.fromCoordinates ( 1, 0 )
    -->         , centerPoint =
    -->             Point2d.fromCoordinates
    -->                 ( -0.8229
    -->                 , -0.8229
    -->                 )
    -->         , sweptAngle = degrees 41.4096
    -->         }
    -->     )

If the start and end points are coincident or the distance between them is more
than twice the given radius, returns `Nothing`:

    Arc2d.fromEndpoints
        { startPoint = p1
        , endPoint = p2
        , radius = 0.5 -- too small!
        , sweptAngle = Arc2d.smallPositive
        }
    --> Nothing

Note that this means it is dangerous to use this function to construct 180
degree arcs (half circles), since in this case due to numerical roundoff the
distance between the two given points may appear to be slightly more than twice
the given radius. In this case it is safer to use a more specialized approach,
such as

    halfCircle =
        Arc2d.with
            { startPoint = firstPoint
            , centerPoint =
                Point2d.midpoint firstPoint secondPoint
            , sweptAngle = degrees 180
            }

(Use `sweptAngle = degrees -180` for a clockwise arc.)

-}
fromEndpoints : { startPoint : Point2d, endPoint : Point2d, radius : Float, sweptAngle : SweptAngle } -> Maybe Arc2d
fromEndpoints { startPoint, endPoint, radius, sweptAngle } =
    let
        chord =
            LineSegment2d.from startPoint endPoint

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
                            case sweptAngle of
                                SmallPositive ->
                                    offsetMagnitude

                                SmallNegative ->
                                    -offsetMagnitude

                                LargeNegative ->
                                    offsetMagnitude

                                LargePositive ->
                                    -offsetMagnitude

                        offset =
                            Vector2d.with
                                { length = offsetDistance
                                , direction = offsetDirection
                                }

                        midpoint =
                            LineSegment2d.midpoint chord

                        centerPoint =
                            Point2d.translateBy offset midpoint

                        halfLength =
                            sqrt squaredHalfLength

                        shortAngle =
                            2 * asin (halfLength / radius)

                        sweptAngleInRadians =
                            case sweptAngle of
                                SmallPositive ->
                                    shortAngle

                                SmallNegative ->
                                    -shortAngle

                                LargePositive ->
                                    2 * pi - shortAngle

                                LargeNegative ->
                                    shortAngle - 2 * pi
                    in
                    with
                        { centerPoint = centerPoint
                        , startPoint = startPoint
                        , sweptAngle = sweptAngleInRadians
                        }
                )
    else
        Nothing


{-| Get the center point of an arc.

    Arc2d.centerPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 1 )

-}
centerPoint : Arc2d -> Point2d
centerPoint (Internal.Arc2d properties) =
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
    --> Point2d.fromCoordinates ( 3, 1 )

-}
startPoint : Arc2d -> Point2d
startPoint (Internal.Arc2d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

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
sweptAngle (Internal.Arc2d properties) =
    properties.sweptAngle


{-| Get the point along an arc at a given parameter value. A parameter value of
0 corresponds to the start point of the arc and a value of 1 corresponds to the
end point.

    Arc2d.pointOn exampleArc 0.5
    --> Point2d.fromCoordinates ( 2.4142, 2.4142 )

-}
pointOn : Arc2d -> Float -> Point2d
pointOn arc parameter =
    let
        angle =
            parameter * sweptAngle arc
    in
    Point2d.rotateAround (centerPoint arc) angle (startPoint arc)


{-| Get the derivative of an arc with respect to a parameter that is 0 at the
start point of the arc and 1 at the end point of the arc.

    Arc2d.derivative exampleArc 0
    --> Vector2d.fromComponents ( 0, 3.1416 )

    Arc2d.derivative exampleArc 1
    --> Vector2d.fromComponents ( -3.1416, 0 )

-}
derivative : Arc2d -> Float -> Vector2d
derivative arc parameter =
    let
        angle =
            parameter * sweptAngle arc

        startDerivative =
            Vector2d.perpendicularTo
                (Vector2d.from (centerPoint arc) (startPoint arc))
                |> Vector2d.scaleBy (sweptAngle arc)
    in
    startDerivative |> Vector2d.rotateBy angle


{-| Evaluate an arc at a given parameter value, returning the point on the arc
at that parameter value and the derivative with respect to that parameter value.

    Arc2d.evaluate exampleArc 0
    --> ( Point2d.fromCoordinates ( 3, 1 )
    --> , Vector2d.fromComponents ( 0, 3.1416 )
    --> )

    Arc2d.evaluate exampleArc 0.5
    --> ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Vector2d.fromComponents ( -2.2214, 2.2214 )
    --> )

    Arc2d.evaluate exampleArc 1
    --> ( Point2d.fromCoordinates ( 1, 3 )
    --> , Vector2d.fromComponents ( -3.1416, 0 )
    --> )

Equivalent to (but more efficient than) calling `pointOn` and `derivative`
separately.

-}
evaluate : Arc2d -> Float -> ( Point2d, Vector2d )
evaluate arc =
    case Direction2d.from (centerPoint arc) (startPoint arc) of
        Just startRadialDirection ->
            let
                startAngle =
                    Direction2d.angle startRadialDirection

                ( centerX, centerY ) =
                    Point2d.coordinates (centerPoint arc)

                arcRadius =
                    radius arc

                arcSweptAngle =
                    sweptAngle arc

                derivativeMagnitude =
                    arcRadius * arcSweptAngle
            in
            \t ->
                let
                    angle =
                        startAngle + t * arcSweptAngle

                    cosAngle =
                        cos angle

                    sinAngle =
                        sin angle
                in
                ( Point2d.fromCoordinates
                    ( centerX + arcRadius * cosAngle
                    , centerY + arcRadius * sinAngle
                    )
                , Vector2d.fromComponents
                    ( -derivativeMagnitude * sinAngle
                    , derivativeMagnitude * cosAngle
                    )
                )

        Nothing ->
            -- Center and start points are coincident, so arc is just a single
            -- point
            always ( centerPoint arc, Vector2d.zero )


numApproximationSegments : Float -> Arc2d -> Int
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

    Arc2d.toPolyline 0.1 exampleArc
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 2.732, 2 )
    -->     , Point2d.fromCoordinates ( 2, 2.732 )
    -->     , Point2d.fromCoordinates ( 1, 3 )
    -->     ]

A tolerance of zero will be treated as infinity (a single line segment will be
returned).

-}
toPolyline : Float -> Arc2d -> Polyline2d
toPolyline tolerance arc =
    let
        numSegments =
            numApproximationSegments tolerance arc

        point index =
            pointOn arc (toFloat index / toFloat numSegments)

        points =
            List.range 0 numSegments |> List.map point
    in
    Polyline2d.fromVertices points


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa.

    Arc2d.reverse exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 3 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , sweptAngle = degrees -90
    -->     }

-}
reverse : Arc2d -> Arc2d
reverse arc =
    with
        { startPoint = endPoint arc
        , centerPoint = centerPoint arc
        , sweptAngle = -(sweptAngle arc)
        }


{-| Scale an arc about a given point by a given scale.

    point =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.scaleAbout point 2 exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 6, 1 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( 2, 1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
scaleAbout : Point2d -> Float -> Arc2d -> Arc2d
scaleAbout point scale arc =
    let
        scalePoint =
            Point2d.scaleAbout point scale
    in
    with
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
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( -1, 3 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( -1, 1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
rotateAround : Point2d -> Float -> Arc2d -> Arc2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
    \arc ->
        with
            { centerPoint = rotatePoint (centerPoint arc)
            , startPoint = rotatePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


{-| Translate an arc by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Arc2d.translateBy displacement exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 5, 4 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( 3, 4 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector2d -> Arc2d -> Arc2d
translateBy displacement arc =
    let
        translatePoint =
            Point2d.translateBy displacement
    in
    with
        { centerPoint = translatePoint (centerPoint arc)
        , startPoint = translatePoint (startPoint arc)
        , sweptAngle = sweptAngle arc
        }


{-| Mirror an arc across a given axis.

    Arc2d.mirrorAcross Axis2d.y exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( -3, 1 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( -1, 1 )
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
        with
            { centerPoint = mirrorPoint (centerPoint arc)
            , startPoint = mirrorPoint (startPoint arc)
            , sweptAngle = -(sweptAngle arc)
            }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.relativeTo localFrame exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 2, -1 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( 0, -1 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame2d -> Arc2d -> Arc2d
relativeTo frame arc =
    let
        relativePoint =
            Point2d.relativeTo frame
    in
    with
        { centerPoint = relativePoint (centerPoint arc)
        , startPoint = relativePoint (startPoint arc)
        , sweptAngle =
            if Frame2d.isRightHanded frame then
                sweptAngle arc
            else
                -(sweptAngle arc)
        }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.placeIn localFrame exampleArc
    --> Arc2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 4, 3 )
    -->     , centerPoint =
    -->         Point2d.fromCoordinates ( 2, 3 )
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame2d -> Arc2d -> Arc2d
placeIn frame arc =
    let
        placePoint =
            Point2d.placeIn frame
    in
    with
        { centerPoint = placePoint (centerPoint arc)
        , startPoint = placePoint (startPoint arc)
        , sweptAngle =
            if Frame2d.isRightHanded frame then
                sweptAngle arc
            else
                -(sweptAngle arc)
        }
