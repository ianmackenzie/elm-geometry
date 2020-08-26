--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Arc2d exposing
    ( Arc2d
    , from, with, sweptAround, throughPoints, withRadius
    , centerPoint, radius, startPoint, midpoint, endPoint, sweptAngle, boundingBox
    , pointOn
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, sample
    , segments, approximate, toPolyline
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , firstDerivative, numApproximationSegments
    )

{-| An `Arc2d` is a section of a circle, defined by its center point, start
point and swept angle (the counterclockwise angle from the start point to the
end point). This module includes functionality for

  - Constructing arcs through given points and/or with a given radius
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc2d


# Constructors

@docs from, with, sweptAround, throughPoints, withRadius


# Properties

@docs centerPoint, radius, startPoint, midpoint, endPoint, sweptAngle, boundingBox


# Evaluation

@docs pointOn
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, sample


# Linear approximation

@docs segments, approximate, toPolyline


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Advanced

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, numApproximationSegments

-}

import Angle exposing (Angle)
import Angle.Interval
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Curve
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity(..), Rate)
import Quantity.Extra as Quantity
import Quantity.Interval as Interval
import SweptAngle exposing (SweptAngle)
import Vector2d exposing (Vector2d)


{-| -}
type alias Arc2d units coordinates =
    Types.Arc2d units coordinates


twoPi : Angle
twoPi =
    Angle.radians (2 * pi)


{-| Construct an arc with from the first given point to the second, with the
given swept angle.

    p1 =
        Point2d.meters 2 1

    p2 =
        Point2d.meters 1 2

    arc1 =
        Arc2d.from p1 p2 (Angle.degrees 90)

    Arc2d.centerPoint arc1
    --> Point2d.meters 1 1

    arc2 =
        Arc2d.from p1 p2 (Angle.degrees -90)

    Arc2d.centerPoint arc2
    --> Point2d.meters 2 2

    arc3 =
        Arc2d.from p1 p2 (Angle.degrees 180)

    Arc2d.centerPoint arc3
    --> Point2d.meters 1.5 1.5

-}
from : Point2d units coordinates -> Point2d units coordinates -> Angle -> Arc2d units coordinates
from givenStartPoint givenEndPoint givenSweptAngle =
    let
        displacement =
            Vector2d.from givenStartPoint givenEndPoint
    in
    case Vector2d.direction displacement of
        Just direction ->
            let
                distance =
                    Vector2d.length displacement

                numTurns =
                    Quantity.ratio givenSweptAngle twoPi

                angleModTwoPi =
                    givenSweptAngle
                        |> Quantity.minus
                            (twoPi
                                |> Quantity.multiplyBy
                                    (toFloat (floor numTurns))
                            )

                halfAngle =
                    Quantity.multiplyBy 0.5 givenSweptAngle

                scale =
                    1 / (2 * abs (Angle.sin halfAngle))

                computedRadius =
                    Quantity.multiplyBy scale distance
            in
            Types.Arc2d
                { startPoint = givenStartPoint
                , sweptAngle = givenSweptAngle
                , xDirection =
                    direction
                        |> Direction2d.rotateBy
                            (Quantity.multiplyBy -0.5 angleModTwoPi)
                , signedLength =
                    if givenSweptAngle == Quantity.zero then
                        distance

                    else
                        Quantity.rTheta computedRadius givenSweptAngle
                }

        Nothing ->
            Types.Arc2d
                { startPoint = givenStartPoint
                , sweptAngle = givenSweptAngle
                , xDirection = Direction2d.x
                , signedLength = Quantity.zero
                }


{-| Construct an arc with the given center point, radius, start angle and swept
angle:

    arc =
        Arc2d.with
            { centerPoint = Point2d.meters 2 0
            , radius = Length.meters 1
            , startAngle = Angle.degrees 45
            , sweptAngle = Angle.degrees -90
            }

    Arc2d.startPoint arc
    --> Point2d.meters 2.7071 0.7071

    Arc2d.endPoint arc
    --> Point2d.meters 2.7071 -0.7071

-}
with : { centerPoint : Point2d units coordinates, radius : Quantity Float units, startAngle : Angle, sweptAngle : Angle } -> Arc2d units coordinates
with properties =
    let
        x0 =
            Point2d.xCoordinate properties.centerPoint

        y0 =
            Point2d.yCoordinate properties.centerPoint

        givenRadius =
            properties.radius

        givenStartAngle =
            properties.startAngle

        givenSweptAngle =
            properties.sweptAngle

        startX =
            x0 |> Quantity.plus (Quantity.rCosTheta givenRadius givenStartAngle)

        startY =
            y0 |> Quantity.plus (Quantity.rSinTheta givenRadius givenStartAngle)
    in
    Types.Arc2d
        { startPoint = Point2d.xy startX startY
        , sweptAngle = givenSweptAngle
        , xDirection = Direction2d.fromAngle (givenStartAngle |> Quantity.plus (Angle.degrees 90))
        , signedLength = Quantity.rTheta (Quantity.abs givenRadius) givenSweptAngle
        }


{-| Construct an arc by sweeping (rotating) a given start point around a given
center point by a given angle. The center point to sweep around is given first
and the start point to be swept is given last.

    exampleArc =
        Point2d.meters 3 1
            |> Arc2d.sweptAround (Point2d.meters 1 1)
                (Angle.degrees 90)

    Arc2d.endPoint exampleArc
    --> Point2d.meters 1 3

A positive swept angle means that the arc is formed by rotating the start point
counterclockwise around the center point. A negative swept angle results in
a clockwise arc instead.

-}
sweptAround : Point2d units coordinates -> Angle -> Point2d units coordinates -> Arc2d units coordinates
sweptAround givenCenterPoint givenSweptAngle givenStartPoint =
    let
        displacement =
            Vector2d.from givenStartPoint givenCenterPoint
    in
    case Vector2d.direction displacement of
        Just yDirection ->
            let
                computedRadius =
                    Vector2d.length displacement
            in
            Types.Arc2d
                { startPoint = givenStartPoint
                , xDirection = yDirection |> Direction2d.rotateClockwise
                , sweptAngle = givenSweptAngle
                , signedLength = Quantity.rTheta computedRadius givenSweptAngle
                }

        Nothing ->
            Types.Arc2d
                { startPoint = givenStartPoint
                , xDirection = Direction2d.x
                , sweptAngle = givenSweptAngle
                , signedLength = Quantity.zero
                }


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point:

    Arc2d.throughPoints
        Point2d.origin
        (Point2d.meters 1 0)
        (Point2d.meters 0 1)
    --> Just
    -->     (Point2d.origin
    -->         |> Arc2d.sweptAround
    -->             (Point2d.meters 0.5 0.5)
    -->             (Angle.degrees 270)
    -->     )

If the three points are collinear, returns `Nothing`:

    Arc2d.throughPoints
        Point2d.origin
        (Point2d.meters 1 0)
        (Point2d.meters 2 0)
    --> Nothing

-}
throughPoints : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Arc2d units coordinates)
throughPoints firstPoint secondPoint thirdPoint =
    Point2d.circumcenter firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\computedCenterPoint ->
                let
                    firstVector =
                        Vector2d.from computedCenterPoint firstPoint

                    secondVector =
                        Vector2d.from computedCenterPoint secondPoint

                    thirdVector =
                        Vector2d.from computedCenterPoint thirdPoint
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

                            computedSweptAngle =
                                if
                                    (partial
                                        |> Quantity.greaterThanOrEqualTo
                                            Quantity.zero
                                    )
                                        && (full
                                                |> Quantity.greaterThanOrEqualTo
                                                    partial
                                           )
                                then
                                    full

                                else if
                                    (partial
                                        |> Quantity.lessThanOrEqualTo
                                            Quantity.zero
                                    )
                                        && (full
                                                |> Quantity.lessThanOrEqualTo
                                                    partial
                                           )
                                then
                                    full

                                else if
                                    full
                                        |> Quantity.greaterThanOrEqualTo
                                            Quantity.zero
                                then
                                    full |> Quantity.minus twoPi

                                else
                                    full |> Quantity.plus twoPi
                        in
                        firstPoint
                            |> sweptAround computedCenterPoint
                                computedSweptAngle
                    )
                    (Vector2d.direction firstVector)
                    (Vector2d.direction secondVector)
                    (Vector2d.direction thirdVector)
            )


{-| Attempt to construct an arc with the given radius between the given start
and end points. Note that this is only possible if the given radius is large
enough! For any given valid radius, start point and end point, there are four
possible results, so the [`SweptAngle`](SweptAngle) argument is used to specify
which arc to create. For example:

    p1 =
        Point2d.meters 1 0

    p2 =
        Point2d.meters 0 1

    Arc2d.withRadius (Length.meters 1)
        SweptAngle.smallPositive
        p1
        p2
    --> Just
    -->     (Point2d.meters 1 0
    -->         |> Arc2d.sweptAround Point2d.origin
    -->             (Angle.degrees 90)
    -->     )

If the start and end points are coincident or the distance between them is more
than twice the given radius, returns `Nothing`:

    -- p1 and p2 are too far apart to be connected by an
    -- arc of radius 0.5
    Arc2d.withRadius (Length.meters 0.5)
        SweptAngle.smallPositive
        p1
        p2
    --> Nothing

Note that this means it is dangerous to use this function to construct 180
degree arcs (half circles), since in this case due to numerical roundoff the
distance between the two given points may appear to be slightly more than twice
the given radius. In this case it is safer to use `Arc2d.from`, such as (for a
counterclockwise arc):

    halfCircle =
        Arc2d.from firstPoint secondPoint <|
            Angle.degrees 180

(Use `Angle.degrees -180` for a clockwise arc.)

-}
withRadius : Quantity Float units -> SweptAngle -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Arc2d units coordinates)
withRadius givenRadius givenSweptAngle givenStartPoint givenEndPoint =
    let
        chord =
            LineSegment2d.from givenStartPoint givenEndPoint

        squaredRadius =
            Quantity.squared givenRadius

        squaredHalfLength =
            LineSegment2d.length chord
                |> Quantity.multiplyBy 0.5
                |> Quantity.squared
    in
    if squaredRadius |> Quantity.greaterThanOrEqualTo squaredHalfLength then
        LineSegment2d.perpendicularDirection chord
            |> Maybe.map
                (\offsetDirection ->
                    let
                        offsetMagnitude =
                            Quantity.sqrt
                                (squaredRadius
                                    |> Quantity.minus squaredHalfLength
                                )

                        offsetDistance =
                            case givenSweptAngle of
                                Types.SmallPositive ->
                                    offsetMagnitude

                                Types.SmallNegative ->
                                    Quantity.negate offsetMagnitude

                                Types.LargeNegative ->
                                    offsetMagnitude

                                Types.LargePositive ->
                                    Quantity.negate offsetMagnitude

                        computedCenterPoint =
                            LineSegment2d.midpoint chord
                                |> Point2d.translateIn offsetDirection offsetDistance

                        halfLength =
                            Quantity.sqrt squaredHalfLength

                        shortAngle =
                            Quantity.ratio halfLength givenRadius
                                |> Angle.asin
                                |> Quantity.multiplyBy 2

                        sweptAngleInRadians =
                            case givenSweptAngle of
                                Types.SmallPositive ->
                                    shortAngle

                                Types.SmallNegative ->
                                    Quantity.negate shortAngle

                                Types.LargePositive ->
                                    twoPi |> Quantity.minus shortAngle

                                Types.LargeNegative ->
                                    shortAngle |> Quantity.minus twoPi
                    in
                    givenStartPoint
                        |> sweptAround computedCenterPoint sweptAngleInRadians
                )

    else
        Nothing


{-| Convert an arc from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Arc2d units1 coordinates -> Arc2d units2 coordinates
at rate (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.at rate arc.startPoint
        , xDirection = arc.xDirection
        , signedLength = Quantity.at rate arc.signedLength
        , sweptAngle = arc.sweptAngle
        }


{-| Convert an arc from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Arc2d units1 coordinates -> Arc2d units2 coordinates
at_ rate arc =
    at (Quantity.inverse rate) arc


{-| Get the center point of an arc.
-}
centerPoint : Arc2d units coordinates -> Point2d units coordinates
centerPoint (Types.Arc2d arc) =
    let
        x0 =
            Point2d.xCoordinate arc.startPoint

        y0 =
            Point2d.yCoordinate arc.startPoint

        dx =
            Direction2d.xComponent arc.xDirection

        dy =
            Direction2d.yComponent arc.xDirection

        r =
            Quantity.lOverTheta arc.signedLength arc.sweptAngle

        cx =
            x0 |> Quantity.minus (Quantity.multiplyBy dy r)

        cy =
            y0 |> Quantity.plus (Quantity.multiplyBy dx r)
    in
    Point2d.xy cx cy


{-| Get the radius of an arc.
-}
radius : Arc2d units coordinates -> Quantity Float units
radius (Types.Arc2d arc) =
    Quantity.lOverTheta arc.signedLength arc.sweptAngle


{-| Get the start point of an arc.
-}
startPoint : Arc2d units coordinates -> Point2d units coordinates
startPoint (Types.Arc2d properties) =
    properties.startPoint


{-| Get the midpoint of an arc.
-}
midpoint : Arc2d units coordinates -> Point2d units coordinates
midpoint arc =
    pointOn arc 0.5


{-| Get the end point of an arc.
-}
endPoint : Arc2d units coordinates -> Point2d units coordinates
endPoint arc =
    pointOn arc 1.0


{-| Get the swept angle of an arc. The result will be positive for a
counterclockwise arc and negative for a clockwise one.
-}
sweptAngle : Arc2d units coordinates -> Angle
sweptAngle (Types.Arc2d properties) =
    properties.sweptAngle


{-| Get a bounding box for a given arc.
-}
boundingBox : Arc2d units coordinates -> BoundingBox2d units coordinates
boundingBox givenArc =
    let
        (Types.Arc2d { xDirection }) =
            givenArc

        theta =
            sweptAngle givenArc
    in
    if Quantity.abs theta |> Quantity.lessThan (Angle.degrees 5) then
        let
            p1 =
                startPoint givenArc

            p2 =
                endPoint givenArc

            offset =
                Quantity.half (Point2d.distanceFrom p1 p2)
                    |> Quantity.divideBy (Angle.cos (Quantity.half theta))

            offsetPoint =
                p1 |> Point2d.translateIn xDirection offset
        in
        BoundingBox2d.hull3 p1 p2 offsetPoint

    else
        let
            startAngle =
                Direction2d.toAngle xDirection
                    |> Quantity.minus (Angle.degrees 90)

            endAngle =
                startAngle |> Quantity.plus theta

            angleInterval =
                Interval.from startAngle endAngle

            cosTheta =
                Angle.Interval.cos angleInterval

            sinTheta =
                Angle.Interval.sin angleInterval

            ( Quantity cosMin, Quantity cosMax ) =
                Interval.endpoints cosTheta

            ( Quantity sinMin, Quantity sinMax ) =
                Interval.endpoints sinTheta

            ( x0, y0 ) =
                Point2d.coordinates (centerPoint givenArc)

            r =
                radius givenArc
        in
        BoundingBox2d.fromExtrema
            { minX = x0 |> Quantity.plus (r |> Quantity.multiplyBy cosMin)
            , maxX = x0 |> Quantity.plus (r |> Quantity.multiplyBy cosMax)
            , minY = y0 |> Quantity.plus (r |> Quantity.multiplyBy sinMin)
            , maxY = y0 |> Quantity.plus (r |> Quantity.multiplyBy sinMax)
            }


{-| Get the point along an arc at a given parameter value.
-}
pointOn : Arc2d units coordinates -> Float -> Point2d units coordinates
pointOn (Types.Arc2d arc) parameterValue =
    let
        x0 =
            Point2d.xCoordinate arc.startPoint

        y0 =
            Point2d.yCoordinate arc.startPoint

        dx =
            Direction2d.xComponent arc.xDirection

        dy =
            Direction2d.yComponent arc.xDirection

        arcSignedLength =
            arc.signedLength

        arcSweptAngle =
            arc.sweptAngle
    in
    if arcSweptAngle == Quantity.zero then
        let
            distance =
                Quantity.multiplyBy parameterValue arcSignedLength

            px =
                x0 |> Quantity.plus (distance |> Quantity.multiplyBy dx)

            py =
                y0 |> Quantity.plus (distance |> Quantity.multiplyBy dy)
        in
        Point2d.xy px py

    else
        let
            theta =
                Quantity.multiplyBy parameterValue arcSweptAngle

            arcRadius =
                Quantity.lOverTheta arcSignedLength arcSweptAngle

            x =
                Quantity.rSinTheta arcRadius theta

            y =
                if
                    Quantity.abs theta
                        |> Quantity.lessThan
                            (Angle.radians (pi / 2))
                then
                    x
                        |> Quantity.multiplyBy
                            (Angle.tan (Quantity.multiplyBy 0.5 theta))

                else
                    Quantity.multiplyBy (1 - Angle.cos theta) arcRadius

            px =
                x0 |> Quantity.plus (Quantity.aXbY dx x -dy y)

            py =
                y0 |> Quantity.plus (Quantity.aXbY dy x dx y)
        in
        Point2d.xy px py


{-| Get the first derivative of an arc at a given parameter value.
-}
firstDerivative : Arc2d units coordinates -> Float -> Vector2d units coordinates
firstDerivative (Types.Arc2d arc) =
    let
        startDerivative =
            Vector2d.withLength arc.signedLength arc.xDirection
    in
    \parameterValue ->
        startDerivative
            |> Vector2d.rotateBy
                (Quantity.multiplyBy parameterValue arc.sweptAngle)


{-| Represents a nondegenerate spline (one that has finite, non-zero length).
-}
type Nondegenerate units coordinates
    = Nondegenerate (Arc2d units coordinates)


{-| Attempt to construct a nondegenerate arc from a general `Arc2d`. If the arc
is in fact degenerate (consists of a single point), returns an `Err` with that
point.
-}
nondegenerate : Arc2d units coordinates -> Result (Point2d units coordinates) (Nondegenerate units coordinates)
nondegenerate arc =
    let
        (Types.Arc2d properties) =
            arc
    in
    if properties.signedLength == Quantity.zero then
        Err (startPoint arc)

    else
        Ok (Nondegenerate arc)


{-| Convert a nondegenerate arc back to a general `Arc2d`.
-}
fromNondegenerate : Nondegenerate units coordinates -> Arc2d units coordinates
fromNondegenerate (Nondegenerate arc) =
    arc


{-| Get the tangent direction to a nondegenerate arc at a given parameter
value.
-}
tangentDirection : Nondegenerate units coordinates -> Float -> Direction2d coordinates
tangentDirection (Nondegenerate (Types.Arc2d arc)) parameterValue =
    arc.xDirection
        |> Direction2d.rotateBy
            (Quantity.multiplyBy parameterValue arc.sweptAngle)


{-| Get both the point and tangent direction of a nondegenerate arc at a given
parameter value.
-}
sample : Nondegenerate units coordinates -> Float -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Approximate an arc by a given number of line segments:

    Arc2d.segments 2 exampleArc
    --> Polyline2d.fromVertices
    -->     [ Point2d.meters 3 1
    -->     , Point2d.meters 2.4142 2.4142
    -->     , Point2d.meters 1 3
    -->     ]

Note that the number of points in the polyline is one more than the number of
segments.

-}
segments : Int -> Arc2d units coordinates -> Polyline2d units coordinates
segments numSegments arc =
    Polyline2d.fromVertices (Parameter1d.steps numSegments (pointOn arc))


{-| Approximate an arc as a polyline, within a given tolerance:

    Arc2d.approximate (Length.meters 0.1) exampleArc
    --> Polyline2d.fromVertices
    -->     [ Point2d.meters 3 1
    -->     , Point2d.meters 2.732 2
    -->     , Point2d.meters 2 2.732
    -->     , Point2d.meters 1 3
    -->     ]

In this example, every point on the returned polyline will be within 0.1 meters
of the original arc.

-}
approximate : Quantity Float units -> Arc2d units coordinates -> Polyline2d units coordinates
approximate maxError arc =
    segments (numApproximationSegments maxError arc) arc


{-| DEPRECATED - use [`segments`](#segments) or [`approximate`](#approximate)
instead.
-}
toPolyline : { maxError : Quantity Float units } -> Arc2d units coordinates -> Polyline2d units coordinates
toPolyline { maxError } arc =
    approximate maxError arc


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa.
-}
reverse : Arc2d units coordinates -> Arc2d units coordinates
reverse ((Types.Arc2d arc) as arc_) =
    Types.Arc2d
        { startPoint = endPoint arc_
        , sweptAngle = Quantity.negate arc.sweptAngle
        , signedLength = Quantity.negate arc.signedLength
        , xDirection = arc.xDirection |> Direction2d.rotateBy arc.sweptAngle
        }


{-| Scale an arc about a given point by a given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> Arc2d units coordinates -> Arc2d units coordinates
scaleAbout point scale (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.scaleAbout point scale arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = Quantity.multiplyBy (abs scale) arc.signedLength
        , xDirection =
            if scale >= 0 then
                arc.xDirection

            else
                Direction2d.reverse arc.xDirection
        }


{-| Rotate an arc around a given point by a given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Arc2d units coordinates -> Arc2d units coordinates
rotateAround point angle (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.rotateAround point angle arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = Direction2d.rotateBy angle arc.xDirection
        }


{-| Translate an arc by a given displacement.
-}
translateBy : Vector2d units coordinates -> Arc2d units coordinates -> Arc2d units coordinates
translateBy displacement (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        }


{-| Translate an arc in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Arc2d units coordinates -> Arc2d units coordinates
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an arc across a given axis. This negates the sign of the arc's
swept angle.
-}
mirrorAcross : Axis2d units coordinates -> Arc2d units coordinates -> Arc2d units coordinates
mirrorAcross axis (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.mirrorAcross axis arc.startPoint
        , sweptAngle = Quantity.negate arc.sweptAngle
        , signedLength = Quantity.negate arc.signedLength
        , xDirection =
            Direction2d.reverse (Direction2d.mirrorAcross axis arc.xDirection)
        }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Arc2d units globalCoordinates -> Arc2d units localCoordinates
relativeTo frame (Types.Arc2d arc) =
    if Frame2d.isRightHanded frame then
        Types.Arc2d
            { startPoint = Point2d.relativeTo frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction2d.relativeTo frame arc.xDirection
            }

    else
        Types.Arc2d
            { startPoint = Point2d.relativeTo frame arc.startPoint
            , sweptAngle = Quantity.negate arc.sweptAngle
            , signedLength = Quantity.negate arc.signedLength
            , xDirection =
                Direction2d.reverse
                    (Direction2d.relativeTo frame arc.xDirection)
            }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Arc2d units localCoordinates -> Arc2d units globalCoordinates
placeIn frame (Types.Arc2d arc) =
    if Frame2d.isRightHanded frame then
        Types.Arc2d
            { startPoint = Point2d.placeIn frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction2d.placeIn frame arc.xDirection
            }

    else
        Types.Arc2d
            { startPoint = Point2d.placeIn frame arc.startPoint
            , sweptAngle = Quantity.negate arc.sweptAngle
            , signedLength = Quantity.negate arc.signedLength
            , xDirection =
                Direction2d.reverse (Direction2d.placeIn frame arc.xDirection)
            }


{-| Determine the number of linear segments needed to approximate an arc to
within a given tolerance.
-}
numApproximationSegments : Quantity Float units -> Arc2d units coordinats -> Int
numApproximationSegments maxError arc =
    Curve.arcApproximationSegments
        { maxError = maxError
        , radius = radius arc
        , sweptAngle = sweptAngle arc
        }
