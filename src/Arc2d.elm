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
    , centerPoint, radius, startPoint, endPoint, sweptAngle
    , pointOn, pointsAt
    , Nondegenerate, nondegenerate, fromNondegenerate
    , tangentDirection, tangentDirectionsAt, sample, samplesAt
    , toPolyline
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , relativeTo, placeIn
    , firstDerivative, firstDerivativesAt
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

@docs centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt

-}

import Angle exposing (Angle)
import Arc.SweptAngle as SweptAngle exposing (SweptAngle)
import Axis2d exposing (Axis2d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Quantity.Extra as Quantity
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
        Point2d.fromCoordinates ( 2, 1 )

    p2 =
        Point2d.fromCoordinates ( 1, 2 )

    arc1 =
        Arc2d.from p1 p2 (degrees 90)

    Arc2d.centerPoint arc1
    --> Point2d.fromCoordinates ( 1, 1 )

    arc2 =
        Arc2d.from p1 p2 (degrees -90)

    Arc2d.centerPoint arc2
    --> Point2d.fromCoordinates ( 2, 2 )

    arc3 =
        Arc2d.from p1 p2 (degrees 180)

    Arc2d.centerPoint arc3
    --> Point2d.fromCoordinates ( 1.5, 1.5 )

    arc4 =
        Arc2d.from p1 p2 (degrees -180)

    Arc2d.centerPoint arc4
    --> Point2d.fromCoordinates ( 1.5, 1.5 )

    arc5 =
        Arc2d.from p1 p2 (degrees 45)

    Arc2d.centerPoint arc5
    --> Point2d.fromCoordinates ( 0.2929, 0.2929 )

-}
from : Point2d units coordinates -> Point2d units coordinates -> Angle -> Arc2d units coordinates
from givenStartPoint givenEndPoint givenSweptAngle =
    let
        displacement =
            Vector2d.from givenStartPoint givenEndPoint
    in
    case Vector2d.lengthAndDirection displacement of
        Just ( distance, direction ) ->
            let
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
            { centerPoint =
                Point2d.fromCoordinates ( 2, 0 )
            , radius = 1
            , startAngle = degrees 45
            , sweptAngle = degrees -90
            }

    Arc2d.startPoint arc
    --> Point2d.fromCoordinates ( 2.7071, 0.7071 )

    Arc2d.endPoint arc
    --> Point2d.fromCoordinates ( 2.7071, -0.7071 )

-}
with : { centerPoint : Point2d units coordinates, radius : Quantity Float units, startAngle : Angle, sweptAngle : Angle } -> Arc2d units coordinates
with properties =
    let
        ( x0, y0 ) =
            Point2d.coordinates properties.centerPoint

        givenRadius =
            properties.radius

        givenStartAngle =
            properties.startAngle

        givenSweptAngle =
            properties.sweptAngle
    in
    Types.Arc2d
        { startPoint =
            Point2d.fromCoordinates
                ( x0
                    |> Quantity.plus
                        (Quantity.rCosTheta givenRadius givenStartAngle)
                , y0
                    |> Quantity.plus
                        (Quantity.rSinTheta givenRadius givenStartAngle)
                )
        , sweptAngle = givenSweptAngle
        , xDirection =
            Direction2d.fromAngle
                (givenStartAngle |> Quantity.plus (Angle.degrees 90))
        , signedLength =
            Quantity.rTheta (Quantity.abs givenRadius) givenSweptAngle
        }


{-| Construct an arc by sweeping (rotating) a given start point around a given
center point by a given angle. The center point to sweep around is given first
and the start point to be swept is given last.

    exampleArc =
        Point2d.fromCoordinates ( 3, 1 )
            |> Arc2d.sweptAround
                (Point2d.fromCoordinates ( 1, 1 ))
                (degrees 90)

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

Note that the 'actual' form of this function is

    arc =
        Arc2d.sweptAround centerPoint sweptAngle startPoint

but it is generally written using the pipe operator `|>` (as in the first
example) to improve readability:

    arc =
        startPoint
            |> Arc2d.sweptAround centerPoint sweptAngle

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
    case Vector2d.lengthAndDirection displacement of
        Just ( computedRadius, yDirection ) ->
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
        (Point2d.fromCoordinates ( 1, 0 ))
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Just
    -->     (Point2d.origin
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -->             (degrees 270)
    -->     )

    Arc2d.throughPoints
        (Point2d.fromCoordinates ( 1, 0 ))
        Point2d.origin
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -->             (degrees -180)
    -->     )

If the three points are collinear, returns `Nothing`:

    Arc2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
        (Point2d.fromCoordinates ( 2, 0 ))
    --> Nothing

    Arc2d.throughPoints
        Point2d.origin
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
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
possible results, so the [`SweptAngle`](Arc-SweptAngle) argument is used to
specify which arc to create. For example:

    p1 =
        Point2d.fromCoordinates ( 1, 0 )

    p2 =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.withRadius 1 SweptAngle.smallPositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround Point2d.origin
    -->             (degrees 90)
    -->     )

    Arc2d.withRadius 1 SweptAngle.smallNegative p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 1, 1 ))
    -->             (degrees -90)
    -->     )

    Arc2d.withRadius 1 SweptAngle.largePositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 1, 1 ))
    -->             (degrees 270)
    -->     )

    Arc2d.withRadius 1 SweptAngle.largeNegative p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround Point2d.origin
    -->             (degrees -270)
    -->     )

    Arc2d.withRadius 2 SweptAngle.smallPositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates
    -->                 ( -0.8229, -0.8229 )
    -->             )
    -->             (degrees 41.4096)
    -->     )

If the start and end points are coincident or the distance between them is more
than twice the given radius, returns `Nothing`:

    -- p1 and p2 are too far apart to be connected by an
    -- arc of radius 0.5
    Arc2d.withRadius 0.5 SweptAngle.smallPositive p1 p2
    --> Nothing

Note that this means it is dangerous to use this function to construct 180
degree arcs (half circles), since in this case due to numerical roundoff the
distance between the two given points may appear to be slightly more than twice
the given radius. In this case it is safer to use `Arc2d.from`, such as (for a
counterclockwise arc):

    halfCircle =
        Arc2d.from firstPoint secondPoint (degrees 180)

(Use `degrees -180` for a clockwise arc.)

-}
withRadius : Quantity Float units -> SweptAngle -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Arc2d units coordinates)
withRadius givenRadius givenSweptAngle givenStartPoint givenEndPoint =
    let
        chord =
            LineSegment2d.from givenStartPoint givenEndPoint

        squaredRadius =
            Quantity.squared givenRadius

        squaredHalfLength =
            Quantity.multiplyBy 0.25 (LineSegment2d.squaredLength chord)
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

                        offset =
                            Vector2d.withLength offsetDistance offsetDirection

                        midpoint =
                            LineSegment2d.midpoint chord

                        computedCenterPoint =
                            Point2d.translateBy offset midpoint

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


{-| Get the center point of an arc.

    Arc2d.centerPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 1 )

-}
centerPoint : Arc2d units coordinates -> Point2d units coordinates
centerPoint (Types.Arc2d arc) =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

        r =
            Quantity.lOverTheta arc.signedLength arc.sweptAngle
    in
    Point2d.fromCoordinates
        ( x0 |> Quantity.minus (Quantity.multiplyBy dy r)
        , y0 |> Quantity.plus (Quantity.multiplyBy dx r)
        )


{-| Get the radius of an arc.

    Arc2d.radius exampleArc
    --> 2

-}
radius : Arc2d units coordinates -> Quantity Float units
radius (Types.Arc2d arc) =
    Quantity.lOverTheta arc.signedLength arc.sweptAngle


{-| Get the start point of an arc.

    Arc2d.startPoint exampleArc
    --> Point2d.fromCoordinates ( 3, 1 )

-}
startPoint : Arc2d units coordinates -> Point2d units coordinates
startPoint (Types.Arc2d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

-}
endPoint : Arc2d units coordinates -> Point2d units coordinates
endPoint arc =
    pointOn arc ParameterValue.one


{-| Get the swept angle of an arc in radians.

    Arc2d.sweptAngle exampleArc
    --> 1.5708

The result will be positive for a counterclockwise arc and negative for a
clockwise one.

-}
sweptAngle : Arc2d units coordinates -> Angle
sweptAngle (Types.Arc2d properties) =
    properties.sweptAngle


{-| Get the point along an arc at a given parameter value:

    Arc2d.pointOn exampleArc ParameterValue.zero
    --> Point2d.fromCoordinates ( 3, 1 )

    Arc2d.pointOn exampleArc ParameterValue.half
    --> Point2d.fromCoordinates ( 2.4142, 2.4142 )

    Arc2d.pointOn exampleArc ParameterValue.one
    --> Point2d.fromCoordinates ( 1, 3 )

-}
pointOn : Arc2d units coordinates -> ParameterValue -> Point2d units coordinates
pointOn (Types.Arc2d arc) parameterValue =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

        arcSignedLength =
            arc.signedLength

        arcSweptAngle =
            arc.sweptAngle

        t =
            ParameterValue.value parameterValue
    in
    if arcSweptAngle == Quantity.zero then
        let
            distance =
                Quantity.multiplyBy t arcSignedLength
        in
        Point2d.fromCoordinates
            ( x0 |> Quantity.plus (Quantity.multiplyBy dx distance)
            , y0 |> Quantity.plus (Quantity.multiplyBy dy distance)
            )

    else
        let
            theta =
                Quantity.multiplyBy t arcSweptAngle

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
        in
        Point2d.fromCoordinates
            ( x0 |> Quantity.plus (Quantity.aXbY dx x -dy y)
            , y0 |> Quantity.plus (Quantity.aXbY dy x dx y)
            )


{-| Get points along an arc at a given set of parameter values:

    exampleArc |> Arc2d.pointsAt (ParameterValue.steps 2)
    --> [ Point2d.fromCoordinates ( 3, 1 )
    --> , Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Point2d.fromCoordinates ( 1, 3 )
    --> ]

-}
pointsAt : List ParameterValue -> Arc2d units coordinates -> List (Point2d units coordinates)
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| Get the first derivative of an arc at a given parameter value:

    Arc2d.firstDerivative exampleArc ParameterValue.zero
    --> Vector2d.fromComponents ( 0, 3.1416 )

    Arc2d.firstDerivative exampleArc ParameterValue.half
    --> Vector2d.fromComponents ( -2.2214, 2.2214 )

    Arc2d.firstDerivative exampleArc ParameterValue.one
    --> Vector2d.fromComponents ( -3.1416, 0 )

-}
firstDerivative : Arc2d units coordinates -> ParameterValue -> Vector2d units coordinates
firstDerivative (Types.Arc2d arc) =
    let
        startDerivative =
            Vector2d.withLength arc.signedLength arc.xDirection
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue
        in
        startDerivative
            |> Vector2d.rotateBy
                (Quantity.multiplyBy t arc.sweptAngle)


{-| Evaluate the first derivative of an arc at a given set of parameter values:

    exampleArc
        |> Arc2d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector2d.fromComponents ( 0, 3.1416 )
    --> , Vector2d.fromComponents ( -2.2214, 2.2214 )
    --> , Vector2d.fromComponents ( -3.1416, 0 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> Arc2d units coordinates -> List (Vector2d units coordinates)
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents an arc that is definitely not degenerate. It
is used as input to functions such as `Arc2d.tangentDirection` and can be
constructed using `Arc2d.nondegenerate`.

-}
type Nondegenerate units coordinates
    = Nondegenerate (Arc2d units coordinates)


{-| Attempt to construct a nondegenerate arc from a general `Arc2d`. If the arc
is in fact degenerate (consists of a single point), returns an `Err` with that
point.

    Arc2d.nondegenerate exampleArc
    --> Ok nondegenerateExampleArc

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

    Arc2d.fromNondegenerate nondegenerateExampleArc
    --> exampleArc

-}
fromNondegenerate : Nondegenerate units coordinates -> Arc2d units coordinates
fromNondegenerate (Nondegenerate arc) =
    arc


{-| Get the tangent direction to a nondegenerate arc at a given parameter
value:

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.zero
    --> Direction2d.fromAngle (degrees 90)

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.half
    --> Direction2d.fromAngle (degrees 135)

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.one
    --> Direction2d.fromAngle (degrees 180)

-}
tangentDirection : Nondegenerate units coordinates -> ParameterValue -> Direction2d coordinates
tangentDirection (Nondegenerate (Types.Arc2d arc)) parameterValue =
    let
        t =
            ParameterValue.value parameterValue
    in
    arc.xDirection
        |> Direction2d.rotateBy
            (Quantity.multiplyBy t arc.sweptAngle)


{-| Get tangent directions to a nondegenerate arc at a given set of parameter
values:

    nondegenerateExampleArc
        |> Arc2d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction2d.fromAngle (degrees 90)
    --> , Direction2d.fromAngle (degrees 135)
    --> , Direction2d.fromAngle (degrees 180)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate units coordinates -> List (Direction2d coordinates)
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| Get both the point and tangent direction of a nondegenerate arc at a given
parameter value:

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.zero
    --> ( Point2d.fromCoordinates ( 3, 1 )
    --> , Direction2d.fromAngle (degrees 90)
    --> )

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.half
    --> ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Direction2d.fromAngle (degrees 135)
    --> )

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.one
    --> ( Point2d.fromCoordinates ( 1, 3 )
    --> , Direction2d.fromAngle (degrees 180)
    --> )

-}
sample : Nondegenerate units coordinates -> ParameterValue -> ( Point2d units coordinates, Direction2d coordinates )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Get points and tangent directions of a nondegenerate arc at a given set of
parameter values:

    nondegenerateExampleArc
        |> Arc2d.samplesAt (ParameterValue.steps 2)
    --> [ ( Point2d.fromCoordinates ( 3, 1 )
    -->   , Direction2d.fromAngle (degrees 90)
    -->   )
    --> , ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    -->   , Direction2d.fromAngle (degrees 135)
    -->   )
    --> , ( Point2d.fromCoordinates ( 1, 3 )
    -->   , Direction2d.fromAngle (degrees 180)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate units coordinates -> List ( Point2d units coordinates, Direction2d coordinates )
samplesAt parameterValues nondegenerateArc =
    List.map (sample nondegenerateArc) parameterValues


numApproximationSegments : Quantity Float units -> Arc2d units coordinates -> Int
numApproximationSegments maxError arc =
    if sweptAngle arc == Quantity.zero then
        1

    else if maxError |> Quantity.lessThanOrEqualTo Quantity.zero then
        0

    else if
        maxError
            |> Quantity.greaterThanOrEqualTo
                (Quantity.multiplyBy 2 (radius arc))
    then
        1

    else
        let
            maxSegmentAngle =
                Quantity.multiplyBy 2
                    (Angle.acos (1 - Quantity.ratio maxError (radius arc)))
        in
        ceiling (Quantity.ratio (Quantity.abs (sweptAngle arc)) maxSegmentAngle)


{-| Approximate an arc as a polyline, within a given tolerance:

    exampleArc |> Arc2d.toPolyline { maxError = 0.1 }
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 2.732, 2 )
    -->     , Point2d.fromCoordinates ( 2, 2.732 )
    -->     , Point2d.fromCoordinates ( 1, 3 )
    -->     ]

In this example, every point on the returned polyline will be within 0.1 units
of the original arc.

-}
toPolyline : { maxError : Quantity Float units } -> Arc2d units coordinates -> Polyline2d units coordinates
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            arc |> pointsAt (ParameterValue.steps numSegments)
    in
    Polyline2d.fromVertices points


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa.

    Arc2d.reverse exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 1, 1 ))
    -->         (degrees -90)

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

    point =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.scaleAbout point 2 exampleArc
    --> Point2d.fromCoordinates ( 6, 1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 2, 1 ))
    -->         (degrees 90)

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

    Arc2d.rotateAround Point2d.origin (degrees 90)
    --> Point2d.fromCoordinates ( -1, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees 90)

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

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Arc2d.translateBy displacement exampleArc
    --> Point2d.fromCoordinates ( 5, 4 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 3, 4 ))
    -->         (degrees 90)

-}
translateBy : Vector2d units coordinates -> Arc2d units coordinates -> Arc2d units coordinates
translateBy displacement (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        }


{-| Translate an arc in a given direction by a given distance;

    Arc2d.translateIn direction distance

is equivalent to

    Arc2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> Arc2d units coordinates -> Arc2d units coordinates
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an arc across a given axis.

    Arc2d.mirrorAcross Axis2d.y exampleArc
    --> Point2d.fromCoordinates ( -3, 1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees -90)

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

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.relativeTo localFrame exampleArc
    --> Point2d.fromCoordinates ( 2, -1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 0, -1 ))
    -->         (degrees 90)

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

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.placeIn localFrame exampleArc
    --> Point2d.fromCoordinates ( 4, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 2, 3 ))
    -->         (degrees 90)

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
