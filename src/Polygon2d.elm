--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polygon2d
    exposing
        ( Polygon2d
        , allEdges
        , allVertices
        , area
        , boundingBox
        , convexHull
        , innerEdges
        , innerVertices
        , mirrorAcross
        , outerEdges
        , outerVertices
        , perimeter
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , singleLoop
        , translateBy
        , translateIn
        , triangulate
        , withHoles
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/icon.svg" alt="Polygon2d" width="160">

A `Polygon2d` represents a closed polygon in 2D, and is defined by a list of
vertices. This module contains a variety of polygon-related functionality, such
as

  - Computing the perimeter and area of polygons
  - Scaling, rotating, translating and mirroring polygons
  - Converting polygons between different coordinate systems

@docs Polygon2d


# Constructors

@docs singleLoop, withHoles, convexHull


# Properties

@docs outerVertices, innerVertices, outerEdges, innerEdges, allVertices, allEdges, perimeter, area, boundingBox


# Transformations

Transforming a polygon is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Triangulation

@docs triangulate

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Internal as Internal
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d.Monotone as Monotone
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import Vector2d exposing (Vector2d)


{-| -}
type alias Polygon2d =
    Internal.Polygon2d


counterclockwiseArea : List Point2d -> Float
counterclockwiseArea vertices =
    case vertices of
        [] ->
            0

        [ single ] ->
            0

        [ first, second ] ->
            0

        first :: second :: rest ->
            let
                segmentArea start end =
                    Triangle2d.counterclockwiseArea
                        (Triangle2d.fromVertices ( first, start, end ))

                segmentAreas =
                    List.map2 segmentArea (second :: rest) rest
            in
            List.sum segmentAreas


makeOuterLoop : List Point2d -> List Point2d
makeOuterLoop vertices =
    if counterclockwiseArea vertices >= 0 then
        vertices
    else
        List.reverse vertices


makeInnerLoop : List Point2d -> List Point2d
makeInnerLoop vertices =
    if counterclockwiseArea vertices <= 0 then
        vertices
    else
        List.reverse vertices


{-| Construct a polygon without holes from a list of its vertices:

    rectangle =
        Polygon2d.singleLoop
            [ Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 3, 1 )
            , Point2d.fromCoordinates ( 3, 2 )
            , Point2d.fromCoordinates ( 1, 2 )
            ]

The last vertex is implicitly considered to be connected back to the first
vertex (you do not have to close the polygon explicitly). Vertices should be
provided in counterclockwise order; if they are provided in clockwise order they
will be reversed.

-}
singleLoop : List Point2d -> Polygon2d
singleLoop vertices =
    Internal.Polygon2d
        { outerLoop = makeOuterLoop vertices
        , innerLoops = []
        }


{-| -}
withHoles : List Point2d -> List (List Point2d) -> Polygon2d
withHoles outerLoop innerLoops =
    Internal.Polygon2d
        { outerLoop = makeOuterLoop outerLoop
        , innerLoops = List.map makeInnerLoop innerLoops
        }


counterclockwiseAround : Point2d -> Point2d -> Point2d -> Bool
counterclockwiseAround origin a b =
    Vector2d.crossProduct (Vector2d.from origin a) (Vector2d.from origin b) >= 0


chainHelp : List Point2d -> List Point2d -> List Point2d
chainHelp acc list =
    case ( acc, list ) of
        ( r1 :: r2 :: rs, x :: xs ) ->
            if counterclockwiseAround r2 r1 x then
                chainHelp (r2 :: rs) (x :: xs)
            else
                chainHelp (x :: acc) xs

        ( _, x :: xs ) ->
            chainHelp (x :: acc) xs

        ( _, [] ) ->
            List.drop 1 acc


chain : List Point2d -> List Point2d
chain =
    chainHelp []


{-| Computes the [convex hull](https://en.wikipedia.org/wiki/Convex_hull) of a list of points. This is an O(n log n) operation.
-}
convexHull : List Point2d -> Polygon2d
convexHull points =
    -- See http://www.algorithmist.com/index.php/Monotone_Chain_Convex_Hull
    -- for a description of the algorithm.
    let
        sorted =
            points |> List.sortBy Point2d.coordinates

        lower =
            chain sorted

        upper =
            chain (List.reverse sorted)
    in
    singleLoop (lower ++ upper)


{-| -}
outerVertices : Polygon2d -> List Point2d
outerVertices (Internal.Polygon2d { outerLoop }) =
    outerLoop


{-| -}
innerVertices : Polygon2d -> List (List Point2d)
innerVertices (Internal.Polygon2d { innerLoops }) =
    innerLoops


{-| -}
allVertices : Polygon2d -> List Point2d
allVertices polygon =
    List.concat (outerVertices polygon :: innerVertices polygon)


loopEdges : List Point2d -> List LineSegment2d
loopEdges vertices =
    case vertices of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment2d.from all (rest ++ [ first ])


{-| -}
outerEdges : Polygon2d -> List LineSegment2d
outerEdges polygon =

    loopEdges (outerVertices polygon)

{-| -}
innerEdges : Polygon2d -> List (List LineSegment2d)
innerEdges polygon =
    List.map loopEdges (innerVertices polygon)


{-| Get the edges of a polygon. This will include an edge from the last point
back to the first point.

    Polygon2d.edges rectangle
    --> [ LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 3, 1 )
    -->     )
    --> , LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 3, 2 )
    -->     )
    --> , LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 3, 2 )
    -->     , Point2d.fromCoordinates ( 1, 2 )
    -->     )
    --> , LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 2 )
    -->     , Point2d.fromCoordinates ( 1, 1 )
    -->     )
    --> ]

-}
allEdges : Polygon2d -> List LineSegment2d
allEdges polygon =
    List.concat (outerEdges polygon :: innerEdges polygon)


{-| Get the perimeter of a polygon (the sum of the lengths of its edges).

    Polygon2d.perimeter rectangle
    --> 6

-}
perimeter : Polygon2d -> Float
perimeter =
    allEdges >> List.map LineSegment2d.length >> List.sum


{-| Get the area of a polygon. This value will never be negative.

    Polygon2d.area rectangle
    --> 2

-}
area : Polygon2d -> Float
area polygon =
    counterclockwiseArea (outerVertices polygon)
        + List.sum (List.map counterclockwiseArea (innerVertices polygon))


{-| Scale a polygon about a given center point by a given scale.

    point =
        Point2d.fromCoordinates ( 2, 1 )

    Polygon2d.scaleAbout point 2 rectangle
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( 0, 1 )
    -->     , Point2d.fromCoordinates ( 4, 1 )
    -->     , Point2d.fromCoordinates ( 4, 3 )
    -->     , Point2d.fromCoordinates ( 0, 3 )
    -->     ]

-}
scaleAbout : Point2d -> Float -> Polygon2d -> Polygon2d
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale) (scale < 0)


{-| Rotate a polygon around the given center point counterclockwise by the given
angle (in radians).

    rectangle
        |> Polygon2d.rotateAround Point2d.origin
            (degrees 90)
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( -1, 1 )
    -->     , Point2d.fromCoordinates ( -1, 3 )
    -->     , Point2d.fromCoordinates ( -2, 3 )
    -->     , Point2d.fromCoordinates ( -2, 1 )
    -->     ]

-}
rotateAround : Point2d -> Float -> Polygon2d -> Polygon2d
rotateAround point angle =
    mapVertices (Point2d.rotateAround point angle) False


{-| Translate a polygon by the given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Polygon2d.translateBy displacement rectangle
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 5, 4 )
    -->     , Point2d.fromCoordinates ( 5, 5 )
    -->     , Point2d.fromCoordinates ( 3, 5 )
    -->     ]

-}
translateBy : Vector2d -> Polygon2d -> Polygon2d
translateBy vector =
    mapVertices (Point2d.translateBy vector) False


{-| Translate a polygon in a given direction by a given distance;

    Polygon2d.translateIn direction distance

is equivalent to

    Polygon2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Polygon2d -> Polygon2d
translateIn direction distance polygon =
    translateBy (Vector2d.withLength distance direction) polygon


{-| Mirror a polygon across the given axis.

    Polygon2d.mirrorAcross Axis2d.x rectangle
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( 1, -1 )
    -->     , Point2d.fromCoordinates ( 3, -1 )
    -->     , Point2d.fromCoordinates ( 3, -2 )
    -->     , Point2d.fromCoordinates ( 1, -2 )
    -->     ]

Note that if a polygon's vertices were in counterclockwise order before
mirroring, they will be in clockwise order afterward, and vice versa.

-}
mirrorAcross : Axis2d -> Polygon2d -> Polygon2d
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis) True


mapVertices : (Point2d -> Point2d) -> Bool -> Polygon2d -> Polygon2d
mapVertices function invert polygon =
    let
        mappedOuterLoop =
            List.map function (outerVertices polygon)

        mappedInnerLoops =
            List.map (List.map function) (innerVertices polygon)
    in
    if invert then
        Internal.Polygon2d
            { outerLoop = List.reverse mappedOuterLoop
            , innerLoops = List.map List.reverse mappedInnerLoops
            }
    else
        Internal.Polygon2d
            { outerLoop = mappedOuterLoop
            , innerLoops = mappedInnerLoops
            }


{-| Take a polygon defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Polygon2d.relativeTo localFrame rectangle
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( 0, -1 )
    -->     , Point2d.fromCoordinates ( 2, -1 )
    -->     , Point2d.fromCoordinates ( 2, 0 )
    -->     , Point2d.fromCoordinates ( 0, 0 )
    -->     ]

-}
relativeTo : Frame2d -> Polygon2d -> Polygon2d
relativeTo frame =
    mapVertices (Point2d.relativeTo frame) (not (Frame2d.isRightHanded frame))


{-| Take a polygon considered to be defined in local coordinates relative
to a given reference frame, and return that polygon expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Polygon2d.placeIn localFrame rectangle
    --> Polygon2d.singleLoop
    -->     [ Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 4, 3 )
    -->     , Point2d.fromCoordinates ( 4, 4 )
    -->     , Point2d.fromCoordinates ( 2, 4 )
    -->     ]

-}
placeIn : Frame2d -> Polygon2d -> Polygon2d
placeIn frame =
    mapVertices (Point2d.placeIn frame) (not (Frame2d.isRightHanded frame))


{-| Get the minimal bounding box containing a given polygon. Returns `Nothing`
if the polygon has no vertices.

    Polygon2d.boundingBox rectangle
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 1
    -->         , maxX = 3
    -->         , minY = 1
    -->         , maxY = 2
    -->         }
    -->     )

-}
boundingBox : Polygon2d -> Maybe BoundingBox2d
boundingBox polygon =
    BoundingBox2d.containingPoints (outerVertices polygon)


{-| Triangulate a polygon. This uses the `TriangularMesh` data types from
[`ianmackenzie/elm-triangular-mesh`](http://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest).
-}
triangulate : Polygon2d -> TriangularMesh Point2d
triangulate polygon =
    Monotone.triangulation polygon
