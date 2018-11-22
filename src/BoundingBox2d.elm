--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox2d exposing
    ( BoundingBox2d
    , fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints
    , extrema, minX, maxX, minY, maxY, dimensions, midX, midY, centerPoint
    , contains, isContainedIn, intersects, overlappingBy, separatedBy
    , scaleAbout, translateBy, translateIn
    )

{-| A `BoundingBox2d` is a rectangular box in 2D defined by its minimum and
maximum X and Y values. It is possible to generate bounding boxes for most
geometric objects; for example, [`Triangle2d.boundingBox`](Triangle2d#boundingBox)
takes a `Triangle2d` and returns a `BoundingBox2d` that contains that triangle.
There are several use cases where it is more efficient to deal with the bounding
box of an object than the object itself, such as:

  - Intersection checking: If (for example) the bounding boxes of a line segment
    and a triangle do not overlap, then the line segment and triangle cannot
    possibly intersect each other. Expensive intersection checking therefore
    only has to be performed for line segments and triangles whose bounding
    boxes _do_ overlap.
  - 2D rendering: When rendering a 2D scene, any object whose bounding box does
    not overlap the viewing area must itself be completely outside the viewing
    area, and therefore does not have to be drawn. This provides a simple form
    of culling.

@docs BoundingBox2d


# Constructors

@docs fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints


# Properties

@docs extrema, minX, maxX, minY, maxY, dimensions, midX, midY, centerPoint, centroid


# Queries

@docs contains, isContainedIn, intersects, overlappingBy, separatedBy


# Transformations

@docs scaleAbout, translateBy, translateIn

-}

import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Squared)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias BoundingBox2d units coordinates =
    Types.BoundingBox2d units coordinates


{-| Construct a bounding box from its minimum and maximum X and Y values:

    exampleBox =
        BoundingBox2d.fromExtrema
            { minX = 3
            , maxX = 8
            , minY = 2
            , maxY = 6
            }

If the minimum and maximum values are provided in the wrong order (for example
if <code>minX&nbsp;>&nbsp;maxX</code>), then they will be swapped so that the
resulting bounding box is valid.

-}
fromExtrema : { minX : Quantity Float units, maxX : Quantity Float units, minY : Quantity Float units, maxY : Quantity Float units } -> BoundingBox2d units coordinates
fromExtrema givenExtrema =
    if
        (givenExtrema.minX |> Quantity.lessThanOrEqualTo givenExtrema.maxX)
            && (givenExtrema.minY |> Quantity.lessThanOrEqualTo givenExtrema.maxY)
    then
        Types.BoundingBox2d givenExtrema

    else
        Types.BoundingBox2d
            { minX = Quantity.min givenExtrema.minX givenExtrema.maxX
            , maxX = Quantity.max givenExtrema.minX givenExtrema.maxX
            , minY = Quantity.min givenExtrema.minY givenExtrema.maxY
            , maxY = Quantity.max givenExtrema.minY givenExtrema.maxY
            }


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    BoundingBox2d.singleton point
    --> BoundingBox2d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 3
    -->     }

-}
singleton : Point2d units coordinates -> BoundingBox2d units coordinates
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    fromExtrema { minX = x, maxX = x, minY = y, maxY = y }


{-| Construct a bounding box with the two given points as two of its corners.
The points can be given in any order and don't have to represent the 'primary'
diagonal of the bounding box.

    firstPoint =
        Point2d.fromCoordinates ( 2, 3 )

    secondPoint =
        Point2d.fromCoordinates ( -1, 5 )

    BoundingBox2d.from firstPoint secondPoint
    --> BoundingBox2d.fromExtrema
    -->     { minX = -1
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 5
    -->     }

-}
from : Point2d units coordinates -> Point2d units coordinates -> BoundingBox2d units coordinates
from firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    fromExtrema
        { minX = Quantity.min x1 x2
        , maxX = Quantity.max x1 x2
        , minY = Quantity.min y1 y2
        , maxY = Quantity.max y1 y2
        }


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox2d.singleton
            (Point2d.fromCoordinates ( 1, 3 ))

    BoundingBox2d.aggregate [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 1,
    -->         , maxX = 8
    -->         , minY = 2
    -->         , maxY = 6
    -->         }
    -->     )

    BoundingBox2d.aggregate [ exampleBox ]
    --> Just exampleBox

    BoundingBox2d.aggregate []
    --> Nothing

If you have exactly two bounding boxes, you can use [`BoundingBox2d.hull`](#hull)
instead (which returns a `BoundingBox2d` instead of a `Maybe BoundingBox2d`).

-}
aggregate : List (BoundingBox2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
aggregate boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    BoundingBox2d.containingPoints
        [ Point2d.fromCoordinates ( 2, 3 )
        , Point2d.fromCoordinates ( -1, 5 )
        , Point2d.fromCoordinates ( 6, 4 )
        ]
    --> Just <|
    -->     BoundingBox2d.fromExtrema
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 3
    -->         , maxY = 5
    -->         }

    BoundingBox2d.containingPoints []
    --> Nothing

-}
containingPoints : List (Point2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
containingPoints points =
    aggregate (List.map singleton points)


{-| Get the minimum and maximum X and Y values of a bounding box in a single
record.

    BoundingBox2d.extrema exampleBox
    --> { minX = 3
    --> , maxX = 8
    --> , minY = 2
    --> , maxY = 6
    --> }

Can be useful when combined with record destructuring, for example


    { minX, maxX, minY, maxY } =
        BoundingBox2d.extrema exampleBox


    --> minX = 3
    --> maxX = 8
    --> minY = 2
    --> maxY = 6

-}
extrema : BoundingBox2d units coordinates -> { minX : Quantity Float units, maxX : Quantity Float units, minY : Quantity Float units, maxY : Quantity Float units }
extrema (Types.BoundingBox2d boundingBoxExtrema) =
    boundingBoxExtrema


{-| Get the minimum X value of a bounding box.

    BoundingBox2d.minX exampleBox
    --> 3

-}
minX : BoundingBox2d units coordinates -> Quantity Float units
minX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox2d.maxX exampleBox
    --> 8

-}
maxX : BoundingBox2d units coordinates -> Quantity Float units
maxX (Types.BoundingBox2d boundingBox) =
    boundingBox.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox2d.minY exampleBox
    --> 2

-}
minY : BoundingBox2d units coordinates -> Quantity Float units
minY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox2d.maxY exampleBox
    --> 6

-}
maxY : BoundingBox2d units coordinates -> Quantity Float units
maxY (Types.BoundingBox2d boundingBox) =
    boundingBox.maxY


{-| Get the X and Y dimensions (width and height) of a bounding box.


    ( width, height ) =
        BoundingBox2d.dimensions exampleBox


    --> width = 5
    --> height = 4

-}
dimensions : BoundingBox2d units coordinates -> ( Quantity Float units, Quantity Float units )
dimensions boundingBox =
    ( maxX boundingBox |> Quantity.minus (minX boundingBox)
    , maxY boundingBox |> Quantity.minus (minY boundingBox)
    )


{-| Get the median X value of a bounding box.

    BoundingBox2d.midX exampleBox
    --> 5.5

-}
midX : BoundingBox2d units coordinates -> Quantity Float units
midX (Types.BoundingBox2d boundingBox) =
    Quantity.interpolateFrom boundingBox.minX boundingBox.maxX 0.5


{-| Get the median Y value of a bounding box.

    BoundingBox2d.midY exampleBox
    --> 4

-}
midY : BoundingBox2d units coordinates -> Quantity Float units
midY (Types.BoundingBox2d boundingBox) =
    Quantity.interpolateFrom boundingBox.minY boundingBox.maxY 0.5


{-| Get the point at the center of a bounding box.

    BoundingBox2d.centerPoint exampleBox
    --> Point2d.fromCoordinates ( 5.5, 4 )

-}
centerPoint : BoundingBox2d units coordinates -> Point2d units coordinates
centerPoint boundingBox =
    Point2d.fromCoordinates ( midX boundingBox, midY boundingBox )


{-| Check if a bounding box contains a particular point.

    point =
        Point2d.fromCoordinates ( 4, 3 )

    BoundingBox2d.contains point exampleBox
    --> True

    BoundingBox2d.contains Point2d.origin exampleBox
    --> False

-}
contains : Point2d units coordinates -> BoundingBox2d units coordinates -> Bool
contains point boundingBox =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    (x |> Quantity.greaterThanOrEqualTo (minX boundingBox))
        && (x |> Quantity.lessThanOrEqualTo (maxX boundingBox))
        && (y |> Quantity.greaterThanOrEqualTo (minY boundingBox))
        && (y |> Quantity.lessThanOrEqualTo (maxY boundingBox))


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox2d.intersects firstBox secondBox

is equivalent to

    BoundingBox2d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersects firstBox secondBox
    --> True

    BoundingBox2d.intersects firstBox thirdBox
    --> False

-}
intersects : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
intersects other boundingBox =
    (minX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (maxX boundingBox |> Quantity.greaterThanOrEqualTo (minX other))
        && (minY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))
        && (maxY boundingBox |> Quantity.greaterThanOrEqualTo (minY other))


overlapAmount : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Maybe (Quantity Float units)
overlapAmount firstBox secondBox =
    let
        xOverlap =
            Quantity.min (maxX firstBox) (maxX secondBox)
                |> Quantity.minus
                    (Quantity.max (minX firstBox) (minX secondBox))

        yOverlap =
            Quantity.min (maxY firstBox) (maxY secondBox)
                |> Quantity.minus
                    (Quantity.max (minY firstBox) (minY secondBox))
    in
    if
        (xOverlap |> Quantity.greaterThanOrEqualTo Quantity.zero)
            && (yOverlap |> Quantity.greaterThanOrEqualTo Quantity.zero)
    then
        Just (Quantity.min xOverlap yOverlap)

    else
        Nothing


squaredSeparationAmount : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Maybe (Quantity Float (Squared units))
squaredSeparationAmount firstBox secondBox =
    let
        xSeparation =
            Quantity.max (minX firstBox) (minX secondBox)
                |> Quantity.minus
                    (Quantity.min (maxX firstBox) (maxX secondBox))

        ySeparation =
            Quantity.max (minY firstBox) (minY secondBox)
                |> Quantity.minus
                    (Quantity.min (maxY firstBox) (maxY secondBox))
    in
    if
        (xSeparation |> Quantity.greaterThan Quantity.zero)
            && (ySeparation |> Quantity.greaterThan Quantity.zero)
    then
        Just
            (Quantity.squared xSeparation
                |> Quantity.plus (Quantity.squared ySeparation)
            )

    else if xSeparation |> Quantity.greaterThan Quantity.zero then
        Just (Quantity.squared xSeparation)

    else if ySeparation |> Quantity.greaterThan Quantity.zero then
        Just (Quantity.squared ySeparation)

    else if xSeparation == Quantity.zero || ySeparation == Quantity.zero then
        Just Quantity.zero

    else
        Nothing


alwaysFalse : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
alwaysFalse firstBox secondBox =
    False


{-| Check if one box overlaps another by less than, greater than or equal to a
given amount. For example, you could implement a tolerant collision check (one
that only returns true if the boxes overlap by at least some small finite
amount, and ignores boxes that just barely touch each other) as

    boxesCollide box1 box2 =
        BoundingBox2d.overlappingBy GT 0.001 box1 box2

This can be read as "`box1` and `box2` are overlapping by greater than 0.001
units". (The [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Overlap is defined as the _minimum_ distance one box would have to move so that
it did not touch the other, and is always positive for any two overlapping
boxes.

Boxes that just touch are considered to have an overlap of zero, which is
distinct from 'no overlap'. Boxes that do not touch or overlap at all are
considered to have an overlap which is less than zero but not comparable to any
negative number.


### Less than

  - `overlappingBy LT 1e-3` will return true if the two boxes overlap by less
    than 0.001 units or if they do not overlap at all (false if they overlap by
    more than 0.001 units).
  - `overlappingBy LT 0` will return true only if the two boxes don't touch or
    overlap at all.
  - `overlappingBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes are separated by, use `separatedBy` instead.


### Greater than

  - `overlappingBy GT 1e-3` will return true if the two boxes overlap by at
    least 0.001 units (false if they overlap by less than that or do not overlap
    at all).
  - `overlappingBy GT 0` will return true if the two boxes overlap by any
    non-zero amount (false if they just touch or do not overlap at all).
  - `overlappingBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes touch or overlap at all (false if they don't overlap, regardless
    of how close they are to overlapping). In this case, though, it would make
    more sense to just user `intersects` instead.


### Equal to

Checking whether two boxes overlap by exactly a given amount is pretty weird and
vulnerable to floating-point roundoff, but is defined as follows:

  - `overlappingBy EQ 1e-3` will return true if the two boxes overlap by exactly
    0.001 units.
  - `overlappingBy EQ 0` will return true if and only if the boxes just touch
    each other.
  - `overlappingBy EQ -1e-3` will always return false.

-}
overlappingBy : Order -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
overlappingBy order tolerance =
    case order of
        LT ->
            if tolerance |> Quantity.greaterThan Quantity.zero then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance |> Quantity.lessThan tolerance

                        Nothing ->
                            True

            else if tolerance == Quantity.zero then
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == Nothing

            else
                alwaysFalse

        GT ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance |> Quantity.greaterThan tolerance

                        Nothing ->
                            False

            else
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                let
                    expected =
                        Just tolerance
                in
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == expected

            else
                alwaysFalse


{-| Check if one box is separated from another by less than, greater than or
equal to a given amount. For example, to perform clash detection between some
objects, you could use `separatedBy` on those objects' bounding boxes as a quick
check to see if the objects had a gap of at least 1 cm between them:

    safelySeparated box1 box2 =
        BoundingBox2d.separatedBy GT 0.01 box1 box2

This can be read as "`box1` and `box2` are separated by greater than 0.01
units". (The [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Separation is defined as the _minimum_ distance one box would have to move
so that it touched the other, and is always positive for any two boxes that do
not touch.

Boxes that just touch are considered to have a separation of zero, which is
distinct from 'no separation'. 'No separation' (overlap) is considered to be
less than zero but not comparable to any negative number.


### Less than

  - `separatedBy LT 1e-3` will return true if the two boxes are separated by
    less than 0.001 units or if they touch or overlap (false if they are
    separated by at least 0.001 units).
  - `separatedBy LT 0` will return true only if the boxes overlap by some
    non-zero amount.
  - `separatedBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes overlap by, use `overlappingBy` instead.


### Greater than

  - `separatedBy GT 1e-3` will return true if the two boxes are separated by at
    least 0.001 units (false if they are separated by less than that or if they
    touch or overlap).
  - `separatedBy GT 0` will return true if the two boxes are separated by any
    non-zero amount (false if they touch or overlap).
  - `separatedBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes just touch or are separated by any amount (false if they overlap
    by any non-zero amount).


### Equal to

Checking whether two boxes are separated by exactly a given amount is pretty
weird and vulnerable to floating-point roundoff, but is defined as follows:

  - `separatedBy EQ 1e-3` will return true if the two boxes are separated by
    exactly 0.001 units.
  - `separatedBy EQ 0` will return true if and only if the boxes just touch each
    other.
  - `separatedBy EQ -3` will always return false.

-}
separatedBy : Order -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
separatedBy order tolerance =
    case order of
        LT ->
            if tolerance |> Quantity.greaterThan Quantity.zero then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance
                                |> Quantity.lessThan
                                    (Quantity.squared tolerance)

                        Nothing ->
                            True

            else if tolerance == Quantity.zero then
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == Nothing

            else
                alwaysFalse

        GT ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance
                                |> Quantity.greaterThan
                                    (Quantity.squared tolerance)

                        Nothing ->
                            False

            else
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance |> Quantity.greaterThanOrEqualTo Quantity.zero then
                let
                    expected =
                        Just (Quantity.squared tolerance)
                in
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == expected

            else
                alwaysFalse


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            }

    innerBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            }

    overlappingBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 12
            }

    BoundingBox2d.isContainedIn outerBox innerBox
    --> True

    BoundingBox2d.isContainedIn outerBox overlappingBox
    --> False

-}
isContainedIn : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Bool
isContainedIn other boundingBox =
    (minX other |> Quantity.lessThanOrEqualTo (minX boundingBox))
        && (maxX boundingBox |> Quantity.lessThanOrEqualTo (maxX other))
        && (minY other |> Quantity.lessThanOrEqualTo (minY boundingBox))
        && (maxY boundingBox |> Quantity.lessThanOrEqualTo (maxY other))


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.hull firstBox secondBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     }

-}
hull : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
hull firstBox secondBox =
    fromExtrema
        { minX = Quantity.min (minX firstBox) (minX secondBox)
        , maxX = Quantity.max (maxX firstBox) (maxX secondBox)
        , minY = Quantity.min (minY firstBox) (minY secondBox)
        , maxY = Quantity.max (maxY firstBox) (maxY secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 2
    -->         , maxX = 4
    -->         , minY = 2
    -->         , maxY = 3
    -->         }
    -->     )

    BoundingBox2d.intersection firstBox thirdBox
    --> Nothing

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 1
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 2
            , minY = 1
            , maxY = 3
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 1
    -->         , maxX = 1
    -->         , minY = 1
    -->         , maxY = 2
    -->         }
    -->     )

-}
intersection : BoundingBox2d units coordinates -> BoundingBox2d units coordinates -> Maybe (BoundingBox2d units coordinates)
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (fromExtrema
                { minX = Quantity.max (minX firstBox) (minX secondBox)
                , maxX = Quantity.min (maxX firstBox) (maxX secondBox)
                , minY = Quantity.max (minY firstBox) (minY secondBox)
                , maxY = Quantity.min (maxY firstBox) (maxY secondBox)
                }
            )

    else
        Nothing


{-| Scale a bounding box about a given point by a given scale.

    point =
        Point2d.fromCoordinates ( 4, 4 )

    BoundingBox2d.scaleAbout point 2 exampleBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 12
    -->     , minY = 0
    -->     , maxY = 8
    -->     }

-}
scaleAbout : Point2d units coordinates -> Float -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
scaleAbout point scale boundingBox =
    let
        ( x0, y0 ) =
            Point2d.coordinates point

        scaledMinX =
            Quantity.scaleAbout x0 scale (minX boundingBox)

        scaledMaxX =
            Quantity.scaleAbout x0 scale (maxX boundingBox)

        scaledMinY =
            Quantity.scaleAbout y0 scale (minY boundingBox)

        scaledMaxY =
            Quantity.scaleAbout y0 scale (maxY boundingBox)
    in
    if scale >= 0 then
        fromExtrema
            { minX = scaledMinX
            , maxX = scaledMaxX
            , minY = scaledMinY
            , maxY = scaledMaxY
            }

    else
        fromExtrema
            { minX = scaledMaxX
            , maxX = scaledMinX
            , minY = scaledMaxY
            , maxY = scaledMinY
            }


{-| Translate a bounding box by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, -3 )

    BoundingBox2d.translateBy displacement exampleBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = 5
    -->     , maxX = 10
    -->     , minY = -1
    -->     , maxY = 3
    -->     }

-}
translateBy : Vector2d units coordinates -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
translateBy displacement boundingBox =
    let
        ( dx, dy ) =
            Vector2d.components displacement
    in
    fromExtrema
        { minX = minX boundingBox |> Quantity.plus dx
        , maxX = maxX boundingBox |> Quantity.plus dx
        , minY = minY boundingBox |> Quantity.plus dy
        , maxY = maxY boundingBox |> Quantity.plus dy
        }


{-| Translate a bounding box in a given direction by a given distance;

    BoundingBox2d.translateIn direction distance

is equivalent to

    BoundingBox2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
translateIn direction distance boundingBox =
    translateBy (Vector2d.withLength distance direction) boundingBox
