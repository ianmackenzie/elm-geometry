module Geometry.ParameterValues
    exposing
        ( ParameterValues
        , clamped
        , filtered
        , foldl
        , foldr
        , forEach
        , inBetween
        , leading
        , map
        , midpoints
        , steps
        , toList
        , trailing
        )

{-| Many things in `elm-geometry` make use of parameter values that vary from 0
to 1. For example, to get a point on a curve, you supply a parameter value
between 0 and 1 - passing 0 will return the start point of the curve, passing 1
will return the end point, and values in between will return points in between.
Similarly, interpolation functions such as [`Point3d.interpolateFrom`](Point3d#interpolateFrom)
take a value between 0 and 1 as an argument. This module helps define ranges of
parameter values between 0 and 1 conveniently and efficiently.

@docs ParameterValues


# Constructors

@docs steps, leading, trailing, inBetween, midpoints


# Evaluation

@docs map, forEach, foldl, foldr


# Conversions

@docs filtered, clamped, toList

-}


{-| Represents a list or range of parameter values.
-}
type ParameterValues
    = Endpoints Int Int Float
    | Midpoints Int Float
    | Values (List Float)
    | Empty


{-| Get the full range of parameter values for a given number of steps,
including 0 and 1:

    ParameterValues.toList (ParameterValues.steps 1)
    --> [ 0, 1 ]

    ParameterValues.toList (ParameterValues.steps 2)
    --> [ 0, 0.5, 1 ]

    ParameterValues.toList (ParameterValues.steps 5)
    --> [ 0, 0.2, 0.4, 0.6. 0.8, 1 ]

Note that the number of parameter values is one greater than the number of
steps!

-}
steps : Int -> ParameterValues
steps n =
    if n < 1 then
        Empty
    else
        Endpoints 0 n (toFloat n)


leading : Int -> ParameterValues
leading n =
    if n < 1 then
        Empty
    else
        Endpoints 0 (n - 1) (toFloat n)


trailing : Int -> ParameterValues
trailing n =
    if n < 1 then
        Empty
    else
        Endpoints 1 n (toFloat n)


inBetween : Int -> ParameterValues
inBetween n =
    if n < 2 then
        Empty
    else
        Endpoints 1 (n - 1) (toFloat n)


midpoints : Int -> ParameterValues
midpoints n =
    if n < 1 then
        Empty
    else
        Midpoints (2 * n - 1) (2 * toFloat n)


isValid : Float -> Bool
isValid value =
    0 <= value && value <= 1 && not (isNaN value)


filtered : List Float -> ParameterValues
filtered values =
    Values (List.filter isValid values)


clamp value =
    if isNaN value then
        value
    else
        Basics.clamp 0 1 value


clamped : List Float -> ParameterValues
clamped values =
    Values (List.map clamp values)


{-| Call the given function for each parameter value, returning a `List` of
results.

    p1 =
        Point2d.origin

    p2 =
        Point2d.fromCoordinates ( 2, 3 )

    Parameter.forEach (ParameterValues.steps 5)
        (Point2d.interpolateFrom p1 p2)
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 0.4, 0.6 )
    --> , Point2d.fromCoordinates ( 0.8, 1.2 )
    --> , Point2d.fromCoordinates ( 1.2, 1.8 )
    --> , Point2d.fromCoordinates ( 1.6, 2.4 )
    --> , Point2d.fromCoordinates ( 2, 3 )
    --> ]

-}
forEach : ParameterValues -> (Float -> a) -> List a
forEach parameterValues function =
    case parameterValues of
        Endpoints startIndex endIndex divisor ->
            endpointsHelp endIndex startIndex divisor function []

        Midpoints endIndex divisor ->
            midpointsHelp endIndex divisor function []

        Values values ->
            List.map function values

        Empty ->
            []


endpointsHelp : Int -> Int -> Float -> (Float -> a) -> List a -> List a
endpointsHelp index startIndex divisor function accumulated =
    let
        newAccumulated =
            function (toFloat index / divisor) :: accumulated
    in
    if index == startIndex then
        newAccumulated
    else
        endpointsHelp (index - 1) startIndex divisor function newAccumulated


midpointsHelp : Int -> Float -> (Float -> a) -> List a -> List a
midpointsHelp index divisor function accumulated =
    let
        newAccumulated =
            function (toFloat index / divisor) :: accumulated
    in
    if index == 1 then
        newAccumulated
    else
        midpointsHelp (index - 2) divisor function newAccumulated


map : (Float -> a) -> ParameterValues -> List a
map function parameterValues =
    forEach parameterValues function


toList : ParameterValues -> List Float
toList parameterValues =
    forEach parameterValues identity


foldl : (Float -> a -> a) -> a -> ParameterValues -> a
foldl accumulator init parameterValues =
    case parameterValues of
        Endpoints startIndex endIndex divisor ->
            foldlEndpoints startIndex endIndex divisor accumulator init

        Midpoints endIndex divisor ->
            foldlMidpoints 1 endIndex divisor accumulator init

        Values values ->
            List.foldl accumulator init values

        Empty ->
            init


foldlEndpoints : Int -> Int -> Float -> (Float -> a -> a) -> a -> a
foldlEndpoints index endIndex divisor accumulator accumulated =
    let
        newAccumulated =
            accumulator (toFloat index / divisor) accumulated
    in
    if index == endIndex then
        newAccumulated
    else
        foldlEndpoints (index + 1) endIndex divisor accumulator newAccumulated


foldlMidpoints : Int -> Int -> Float -> (Float -> a -> a) -> a -> a
foldlMidpoints index endIndex divisor accumulator accumulated =
    let
        newAccumulated =
            accumulator (toFloat index / divisor) accumulated
    in
    if index == endIndex then
        newAccumulated
    else
        foldlMidpoints (index + 2) endIndex divisor accumulator newAccumulated


foldr : (Float -> a -> a) -> a -> ParameterValues -> a
foldr accumulator init parameterValues =
    case parameterValues of
        Endpoints startIndex endIndex divisor ->
            foldrEndpoints endIndex startIndex divisor accumulator init

        Midpoints endIndex divisor ->
            foldrMidpoints endIndex divisor accumulator init

        Values values ->
            List.foldr accumulator init values

        Empty ->
            init


foldrEndpoints : Int -> Int -> Float -> (Float -> a -> a) -> a -> a
foldrEndpoints index startIndex divisor accumulator accumulated =
    let
        newAccumulated =
            accumulator (toFloat index / divisor) accumulated
    in
    if index == startIndex then
        newAccumulated
    else
        foldrEndpoints (index - 1) startIndex divisor accumulator newAccumulated


foldrMidpoints : Int -> Float -> (Float -> a -> a) -> a -> a
foldrMidpoints index divisor accumulator accumulated =
    let
        newAccumulated =
            accumulator (toFloat index / divisor) accumulated
    in
    if index == 1 then
        newAccumulated
    else
        foldrMidpoints (index - 2) divisor accumulator newAccumulated
