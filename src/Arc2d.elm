module Arc2d
    exposing
        ( Arc2d
        , centerPoint
        , derivativeVector
        , derivativeVectors
        , endPoint
        , from
        , mirrorAcross
        , placeIn
        , pointOn
        , pointsOn
        , radius
        , relativeTo
        , reverse
        , rotateAround
        , sample
        , samples
        , scaleAbout
        , startPoint
        , sweptAngle
        , sweptAround
        , throughPoints
        , toPolyline
        , translateBy
        , translateIn
        , with
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Arc2d/icon.svg" alt="Arc2d" width="160">

An `Arc2d` is a section of a circle, defined by its center point, start
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

@docs pointOn, pointsOn, derivativeVector, derivativeVectors, sample, samples


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Accuracy exposing (Accuracy)
import Geometry.Parameter as Parameter
import Geometry.SweptAngle as SweptAngle exposing (SweptAngle)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Arc2d =
    Types.Arc2d


twoPi : Float
twoPi =
    2 * pi


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
from : Point2d -> Point2d -> Float -> Arc2d
from startPoint_ endPoint_ sweptAngle_ =
    let
        displacement =
            Vector2d.from startPoint_ endPoint_
    in
    case Vector2d.lengthAndDirection displacement of
        Just ( distance, direction ) ->
            let
                angleModTwoPi =
                    sweptAngle_ - twoPi * toFloat (floor (sweptAngle_ / twoPi))

                radius_ =
                    distance / (2 * abs (sin (sweptAngle_ / 2)))
            in
            Types.Arc2d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , xDirection =
                    direction |> Direction2d.rotateBy (-angleModTwoPi / 2)
                , signedLength =
                    if sweptAngle_ == 0.0 then
                        distance
                    else
                        radius_ * sweptAngle_
                }

        Nothing ->
            Types.Arc2d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , xDirection = Direction2d.x
                , signedLength = 0
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
with : { centerPoint : Point2d, radius : Float, startAngle : Float, sweptAngle : Float } -> Arc2d
with properties =
    let
        ( x0, y0 ) =
            Point2d.coordinates properties.centerPoint
    in
    Types.Arc2d
        { startPoint =
            Point2d.fromCoordinates
                ( x0 + properties.radius * cos properties.startAngle
                , y0 + properties.radius * sin properties.startAngle
                )
        , sweptAngle = properties.sweptAngle
        , xDirection =
            Direction2d.fromAngle (properties.startAngle + degrees 90)
        , signedLength = abs properties.radius * properties.sweptAngle
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
sweptAround : Point2d -> Float -> Point2d -> Arc2d
sweptAround centerPoint_ sweptAngle_ startPoint_ =
    case Vector2d.lengthAndDirection (Vector2d.from startPoint_ centerPoint_) of
        Just ( radius_, yDirection ) ->
            Types.Arc2d
                { startPoint = startPoint_
                , xDirection = yDirection |> Direction2d.rotateClockwise
                , sweptAngle = sweptAngle_
                , signedLength = radius_ * sweptAngle_
                }

        Nothing ->
            Types.Arc2d
                { startPoint = startPoint_
                , xDirection = Direction2d.x
                , sweptAngle = sweptAngle_
                , signedLength = 0
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
throughPoints : Point2d -> Point2d -> Point2d -> Maybe Arc2d
throughPoints firstPoint secondPoint thirdPoint =
    Point2d.circumcenter firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\centerPoint_ ->
                let
                    firstVector =
                        Vector2d.from centerPoint_ firstPoint

                    secondVector =
                        Vector2d.from centerPoint_ secondPoint

                    thirdVector =
                        Vector2d.from centerPoint_ thirdPoint
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

                            sweptAngle_ =
                                if partial >= 0 && full >= partial then
                                    full
                                else if partial <= 0 && full <= partial then
                                    full
                                else if full >= 0 then
                                    full - 2 * pi
                                else
                                    full + 2 * pi
                        in
                        firstPoint |> sweptAround centerPoint_ sweptAngle_
                    )
                    (Vector2d.direction firstVector)
                    (Vector2d.direction secondVector)
                    (Vector2d.direction thirdVector)
            )


{-| Attempt to construct an arc with the given radius between the given start
and end points. Note that this is only possible if the given radius is large
enough! For any given valid radius, start point and end point, there are four
possible results, so the second argument is used to specify which arc to create:

  - `SweptAngle.smallPositive` will result in a counterclockwise arc with a
    small swept angle (in the range 0..180 degrees)
  - `SweptAngle.smallNegative` will result in a clockwise arc with a small swept
    angle (in the range -180..0 degrees)
  - `SweptAngle.largePositive` will result in a counterclockwise arc with a
    large swept angle (in the range 180..360 degrees)
  - `SweptAngle.largeNegative` will result in a clockwise arc with a large swept
    angle (in the range -360..-180 degrees)

For example:

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
withRadius : Float -> SweptAngle -> Point2d -> Point2d -> Maybe Arc2d
withRadius radius_ sweptAngle_ startPoint_ endPoint_ =
    let
        chord =
            LineSegment2d.from startPoint_ endPoint_

        squaredRadius =
            radius_ * radius_

        squaredHalfLength =
            LineSegment2d.squaredLength chord / 4
    in
    if squaredRadius >= squaredHalfLength then
        LineSegment2d.perpendicularDirection chord
            |> Maybe.map
                (\offsetDirection ->
                    let
                        offsetMagnitude =
                            sqrt (squaredRadius - squaredHalfLength)

                        offsetDistance =
                            case sweptAngle_ of
                                Types.SmallPositive ->
                                    offsetMagnitude

                                Types.SmallNegative ->
                                    -offsetMagnitude

                                Types.LargeNegative ->
                                    offsetMagnitude

                                Types.LargePositive ->
                                    -offsetMagnitude

                        offset =
                            Vector2d.withLength offsetDistance offsetDirection

                        midpoint =
                            LineSegment2d.midpoint chord

                        centerPoint_ =
                            Point2d.translateBy offset midpoint

                        halfLength =
                            sqrt squaredHalfLength

                        shortAngle =
                            2 * asin (halfLength / radius_)

                        sweptAngleInRadians =
                            case sweptAngle_ of
                                Types.SmallPositive ->
                                    shortAngle

                                Types.SmallNegative ->
                                    -shortAngle

                                Types.LargePositive ->
                                    2 * pi - shortAngle

                                Types.LargeNegative ->
                                    shortAngle - 2 * pi
                    in
                    startPoint_ |> sweptAround centerPoint_ sweptAngleInRadians
                )
    else
        Nothing


{-| Get the center point of an arc.

    Arc2d.centerPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 1 )

-}
centerPoint : Arc2d -> Point2d
centerPoint (Types.Arc2d arc) =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

        r =
            arc.signedLength / arc.sweptAngle
    in
    Point2d.fromCoordinates ( x0 - r * dy, y0 + r * dx )


{-| Get the radius of an arc.

    Arc2d.radius exampleArc
    --> 2

-}
radius : Arc2d -> Float
radius (Types.Arc2d arc) =
    arc.signedLength / arc.sweptAngle


{-| Get the start point of an arc.

    Arc2d.startPoint exampleArc
    --> Point2d.fromCoordinates ( 3, 1 )

-}
startPoint : Arc2d -> Point2d
startPoint (Types.Arc2d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

-}
endPoint : Arc2d -> Point2d
endPoint arc =
    case pointOn arc 1.0 of
        Just point ->
            point

        Nothing ->
            startPoint arc


{-| Get the swept angle of an arc in radians.

    Arc2d.sweptAngle exampleArc
    --> 1.5708

The result will be positive for a counterclockwise arc and negative for a
clockwise one.

-}
sweptAngle : Arc2d -> Float
sweptAngle (Types.Arc2d properties) =
    properties.sweptAngle


{-| Get the point along an arc at a given parameter value. A parameter value of
0 corresponds to the start point of the arc and a value of 1 corresponds to the
end point. Only parameter values from 0 to 1 (inclusive) are valid; passing an
invalid parameter value will result in `Nothing`.

    Arc2d.pointOn exampleArc 0.5
    --> Just (Point2d.fromCoordinates ( 2.4142, 2.4142 ))

    Arc2d.pointOn exampleArc 1.5
    --> Nothing

-}
pointOn : Arc2d -> Float -> Maybe Point2d
pointOn (Types.Arc2d arc) =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

        arcSignedLength =
            arc.signedLength

        arcSweptAngle =
            arc.sweptAngle
    in
    if arcSweptAngle == 0.0 then
        \t ->
            if 0 <= t && t <= 1 then
                let
                    distance =
                        t * arcSignedLength
                in
                Just <|
                    Point2d.fromCoordinates
                        ( x0 + distance * dx
                        , y0 + distance * dy
                        )
            else
                Nothing
    else
        let
            arcRadius =
                arcSignedLength / arcSweptAngle
        in
        \t ->
            if 0 <= t && t <= 1 then
                let
                    theta =
                        t * arcSweptAngle

                    x =
                        arcRadius * sin theta

                    y =
                        if abs theta < pi / 2 then
                            x * tan (theta / 2)
                        else
                            arcRadius * (1 - cos theta)
                in
                Just <|
                    Point2d.fromCoordinates
                        ( x0 + x * dx - y * dy
                        , y0 + x * dy + y * dx
                        )
            else
                Nothing


{-| Convenient shorthand for evaluating multiple points;

    Arc2d.pointsOn arc parameterValues

is equivalent to

    List.map (Arc2d.pointOn arc) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
pointsOn : Arc2d -> List Float -> List Point2d
pointsOn arc parameterValues =
    List.filterMap (pointOn arc) parameterValues


{-| Get the derivative vector of an arc with respect to a parameter that is 0 at
the start point of the arc and 1 at the end point of the arc.

    Arc2d.derivativeVector exampleArc 0
    --> Just (Vector2d.fromComponents ( 0, 3.1416 ))

    Arc2d.derivativeVector exampleArc 1
    --> Just (Vector2d.fromComponents ( -3.1416, 0 ))

-}
derivativeVector : Arc2d -> Float -> Maybe Vector2d
derivativeVector (Types.Arc2d arc) =
    let
        startDerivative =
            Vector2d.withLength arc.signedLength arc.xDirection
    in
    \t ->
        if 0 <= t && t <= 1 then
            Just (startDerivative |> Vector2d.rotateBy (t * arc.sweptAngle))
        else
            Nothing


{-| Convenient shorthand for evaluating multiple derivative vectors;

    Arc2d.derivativeVectors arc parameterValues

is equivalent to

    List.filterMap (Arc2d.derivativeVector arc)
        parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
derivativeVectors : Arc2d -> List Float -> List Vector2d
derivativeVectors arc parameterValues =
    List.filterMap (derivativeVector arc) parameterValues


{-| Sample an arc at a given parameter value to get both the position and
derivative vector at that parameter value. Equivalent to calling `pointOn` and
`derivative` separately.

    Arc2d.sample exampleArc 0
    --> ( Point2d.fromCoordinates ( 3, 1 )
    --> , Vector2d.fromComponents ( 0, 3.1416 )
    --> )

    Arc2d.sample exampleArc 0.5
    --> ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Vector2d.fromComponents ( -2.2214, 2.2214 )
    --> )

    Arc2d.sample exampleArc 1
    --> ( Point2d.fromCoordinates ( 1, 3 )
    --> , Vector2d.fromComponents ( -3.1416, 0 )
    --> )

-}
sample : Arc2d -> Float -> Maybe ( Point2d, Vector2d )
sample arc =
    let
        pointOnArc =
            pointOn arc

        derivativeOfArc =
            derivativeVector arc
    in
    \t -> Maybe.map2 (\p v -> ( p, v )) (pointOnArc t) (derivativeOfArc t)


{-| Convenient shorthand for evaluating multiple samples;

    Arc2d.samples arc parameterValues

is equivalent to

    List.map (Arc2d.sample arc) parameterValues

To generate evenly-spaced parameter values, check out the [`Parameter`](Geometry-Parameter)
module.

-}
samples : Arc2d -> List Float -> List ( Point2d, Vector2d )
samples arc parameterValues =
    List.filterMap (sample arc) parameterValues


numApproximationSegments : Float -> Arc2d -> Int
numApproximationSegments tolerance arc =
    if sweptAngle arc == 0 then
        1
    else if tolerance <= 0 then
        0
    else if tolerance >= 2 * radius arc then
        1
    else
        let
            maxSegmentAngle =
                2 * acos (1 - tolerance / radius arc)
        in
        ceiling (abs (sweptAngle arc) / maxSegmentAngle)


{-| Approximate an arc as a polyline.

    Arc2d.toPolyline (Accuracy.maxError 0.1) exampleArc
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 2.732, 2 )
    -->     , Point2d.fromCoordinates ( 2, 2.732 )
    -->     , Point2d.fromCoordinates ( 1, 3 )
    -->     ]

The accuracy of the approximation is controlled by the first argument; in the
above example, every point on the returned polyline will be within 0.1 units of
the original arc.

-}
toPolyline : Accuracy -> Arc2d -> Polyline2d
toPolyline (Types.MaxError tolerance) arc =
    let
        numSegments =
            numApproximationSegments tolerance arc

        points =
            pointsOn arc (Parameter.numSteps numSegments)
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
reverse : Arc2d -> Arc2d
reverse ((Types.Arc2d arc) as arc_) =
    Types.Arc2d
        { startPoint = endPoint arc_
        , sweptAngle = -arc.sweptAngle
        , signedLength = -arc.signedLength
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
scaleAbout : Point2d -> Float -> Arc2d -> Arc2d
scaleAbout point scale (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.scaleAbout point scale arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = abs scale * arc.signedLength
        , xDirection =
            if scale >= 0 then
                arc.xDirection
            else
                Direction2d.flip arc.xDirection
        }


{-| Rotate an arc around a given point by a given angle.

    Arc2d.rotateAround Point2d.origin (degrees 90)
    --> Point2d.fromCoordinates ( -1, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees 90)

-}
rotateAround : Point2d -> Float -> Arc2d -> Arc2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \(Types.Arc2d arc) ->
        Types.Arc2d
            { startPoint = rotatePoint arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = rotateDirection arc.xDirection
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
translateBy : Vector2d -> Arc2d -> Arc2d
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
translateIn : Direction2d -> Float -> Arc2d -> Arc2d
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an arc across a given axis.

    Arc2d.mirrorAcross Axis2d.y exampleArc
    --> Point2d.fromCoordinates ( -3, 1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees -90)

-}
mirrorAcross : Axis2d -> Arc2d -> Arc2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
    \(Types.Arc2d arc) ->
        Types.Arc2d
            { startPoint = mirrorPoint arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection = Direction2d.flip (mirrorDirection arc.xDirection)
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
relativeTo : Frame2d -> Arc2d -> Arc2d
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
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction2d.flip (Direction2d.relativeTo frame arc.xDirection)
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
placeIn : Frame2d -> Arc2d -> Arc2d
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
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction2d.flip (Direction2d.placeIn frame arc.xDirection)
            }
