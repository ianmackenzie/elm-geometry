--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polygon2d exposing
    ( Polygon2d
    , singleLoop, withHoles, convexHull
    , outerLoop, innerLoops, vertices, edges, perimeter, area, centroid, boundingBox
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    , triangulate, triangulateWith, TriangulationRule, maxEdgeLength, maxTriangleDimensions
    )

{-| A `Polygon2d` represents a closed polygon in 2D, optionally with holes. It
is defined by an outer loop of vertices and a list of inner loops defining any
holes. This module contains a variety of polygon-related functionality, such as

  - Computing the perimeter and area of polygons
  - Scaling, rotating, translating and mirroring polygons
  - Converting polygons between different coordinate systems
  - Triangulating polygons

@docs Polygon2d


# Constructors

@docs singleLoop, withHoles, convexHull


# Properties

@docs outerLoop, innerLoops, vertices, edges, perimeter, area, centroid, boundingBox


# Queries

@docs contains


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Triangulation

@docs triangulate, triangulateWith, TriangulationRule, maxEdgeLength, maxTriangleDimensions

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d.Monotone as Monotone
import Polygon2d.Refinement as Refinement
import Quantity exposing (Quantity(..), Rate, Squared)
import Quantity.Extra as Quantity
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import Vector2d exposing (Vector2d)


{-| -}
type alias Polygon2d units coordinates =
    Types.Polygon2d units coordinates


counterclockwiseArea : List (Point2d units coordinates) -> Quantity Float (Squared units)
counterclockwiseArea vertices_ =
    case vertices_ of
        [] ->
            Quantity.zero

        [ single ] ->
            Quantity.zero

        [ first, second ] ->
            Quantity.zero

        first :: second :: rest ->
            let
                segmentArea start end =
                    Triangle2d.counterclockwiseArea
                        (Triangle2d.from first start end)

                segmentAreas =
                    List.map2 segmentArea (second :: rest) rest
            in
            Quantity.sum segmentAreas


makeOuterLoop : List (Point2d units coordinates) -> List (Point2d units coordinates)
makeOuterLoop vertices_ =
    if
        counterclockwiseArea vertices_
            |> Quantity.greaterThanOrEqualTo Quantity.zero
    then
        vertices_

    else
        List.reverse vertices_


makeInnerLoop : List (Point2d units coordinates) -> List (Point2d units coordinates)
makeInnerLoop vertices_ =
    if
        counterclockwiseArea vertices_
            |> Quantity.lessThanOrEqualTo Quantity.zero
    then
        vertices_

    else
        List.reverse vertices_


{-| Construct a polygon without holes from a list of its vertices:

    rectangle =
        Polygon2d.singleLoop
            [ Point2d.meters 1 1
            , Point2d.meters 3 1
            , Point2d.meters 3 2
            , Point2d.meters 1 2
            ]

The last vertex is implicitly considered to be connected back to the first
vertex (you do not have to close the polygon explicitly). Vertices should
ideally be provided in counterclockwise order; if they are provided in clockwise
order they will be reversed.

-}
singleLoop : List (Point2d units coordinates) -> Polygon2d units coordinates
singleLoop givenOuterLoop =
    withHoles [] givenOuterLoop


{-| Construct a polygon with holes from one outer loop and a list of inner
loops. The loops must not touch or intersect each other.

    outerLoop =
        [ Point2d.meters 0 0
        , Point2d.meters 3 0
        , Point2d.meters 3 3
        , Point2d.meters 0 3
        ]

    innerLoop =
        [ Point2d.meters 1 1
        , Point2d.meters 1 2
        , Point2d.meters 2 2
        , Point2d.meters 2 1
        ]

    squareWithHole =
        Polygon2d.withHoles [ innerLoop ] outerLoop

As with `Polygon2d.singleLoop`, the last vertex of each loop is considered to be
connected back to the first. Vertices of the outer loop should ideally be
provided in counterclockwise order and vertices of the inner loops should
ideally be provided in clockwise order.

-}
withHoles : List (List (Point2d units coordinates)) -> List (Point2d units coordinates) -> Polygon2d units coordinates
withHoles givenInnerLoops givenOuterLoop =
    Types.Polygon2d
        { outerLoop = makeOuterLoop givenOuterLoop
        , innerLoops = List.map makeInnerLoop givenInnerLoops
        }


counterclockwiseAround : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Bool
counterclockwiseAround origin a b =
    let
        crossProduct =
            Vector2d.from origin a
                |> Vector2d.cross
                    (Vector2d.from origin b)
    in
    crossProduct |> Quantity.greaterThanOrEqualTo Quantity.zero


chainHelp : List (Point2d units coordinates) -> List (Point2d units coordinates) -> List (Point2d units coordinates)
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


chain : List (Point2d units coordinates) -> List (Point2d units coordinates)
chain =
    chainHelp []


compareCoordinates : Point2d units coordinates -> Point2d units coordinates -> Order
compareCoordinates firstPoint secondPoint =
    if Point2d.xCoordinate firstPoint == Point2d.xCoordinate secondPoint then
        Quantity.compare (Point2d.yCoordinate firstPoint) (Point2d.yCoordinate secondPoint)

    else
        Quantity.compare (Point2d.xCoordinate firstPoint) (Point2d.xCoordinate secondPoint)


{-| Build the [convex hull](https://en.wikipedia.org/wiki/Convex_hull) of a list
of points:

![Convex hull of a set of points](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/convexHull.svg)

This is an O(n log n) operation.

-}
convexHull : List (Point2d units coordinates) -> Polygon2d units coordinates
convexHull points =
    -- See http://www.algorithmist.com/index.php/Monotone_Chain_Convex_Hull
    -- for a description of the algorithm.
    let
        sorted =
            points |> List.sortWith compareCoordinates

        lower =
            chain sorted

        upper =
            chain (List.reverse sorted)
    in
    singleLoop (lower ++ upper)


{-| Convert a polygon from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Polygon2d units1 coordinates -> Polygon2d units2 coordinates
at rate (Types.Polygon2d polygon) =
    let
        convertPoint =
            Point2d.at rate
    in
    Types.Polygon2d
        { outerLoop = List.map convertPoint polygon.outerLoop
        , innerLoops = List.map (List.map convertPoint) polygon.innerLoops
        }


{-| Convert a polygon from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Polygon2d units1 coordinates -> Polygon2d units2 coordinates
at_ rate polygon =
    at (Quantity.inverse rate) polygon


{-| Get the list of vertices definining the outer loop (border) of a polygon.
The vertices will be in counterclockwise order.

    Polygon2d.outerLoop squareWithHole
    --> [ Point2d.meters 0 0
    --> , Point2d.meters 3 0
    --> , Point2d.meters 3 3
    --> , Point2d.meters 0 3
    --> ]

-}
outerLoop : Polygon2d units coordinates -> List (Point2d units coordinates)
outerLoop (Types.Polygon2d polygon) =
    polygon.outerLoop


{-| Get the holes (if any) of a polygon, each defined by a list of vertices.
Each list of vertices will be in clockwise order.

    Polygon2d.innerLoops squareWithHole
    --> [ [ Point2d.meters 1 1
    -->   , Point2d.meters 1 2
    -->   , Point2d.meters 2 2
    -->   , Point2d.meters 2 1
    -->   ]
    --> ]

-}
innerLoops : Polygon2d units coordinates -> List (List (Point2d units coordinates))
innerLoops (Types.Polygon2d polygon) =
    polygon.innerLoops


{-| Get all vertices of a polygon; this will include vertices from the outer
loop of the polygon and all inner loops. The order of the returned vertices is
undefined.
-}
vertices : Polygon2d units coordinates -> List (Point2d units coordinates)
vertices polygon =
    List.concat (outerLoop polygon :: innerLoops polygon)


loopEdges : List (Point2d units coordinates) -> List (LineSegment2d units coordinates)
loopEdges vertices_ =
    case vertices_ of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment2d.from all (rest ++ [ first ])


{-| Get all edges of a polygon. This will include both outer edges and inner
(hole) edges.
-}
edges : Polygon2d units coordinates -> List (LineSegment2d units coordinates)
edges polygon =
    let
        outerEdges =
            loopEdges (outerLoop polygon)

        innerEdges =
            List.map loopEdges (innerLoops polygon)
    in
    List.concat (outerEdges :: innerEdges)


{-| Get the perimeter of a polygon (the sum of the lengths of its edges). This
includes the outer perimeter and the perimeter of any holes.

    Polygon2d.perimeter squareWithHole
    --> Length.meters 16

-}
perimeter : Polygon2d units coordinates -> Quantity Float units
perimeter =
    edges >> List.map LineSegment2d.length >> Quantity.sum


{-| Get the area of a polygon. This value will never be negative.

    Polygon2d.area squareWithHole
    --> Area.squareMeters 8

-}
area : Polygon2d units coordinates -> Quantity Float (Squared units)
area polygon =
    counterclockwiseArea (outerLoop polygon)
        |> Quantity.plus
            (Quantity.sum (List.map counterclockwiseArea (innerLoops polygon)))


{-| Get the centroid of a polygon. Returns `Nothing` if the polygon has no
vertices or zero area.
-}
centroid : Polygon2d units coordinates -> Maybe (Point2d units coordinates)
centroid polygon =
    case outerLoop polygon of
        first :: _ :: _ ->
            let
                offset =
                    Point2d.unwrap first
            in
            centroidHelp
                offset.x
                offset.y
                first
                (outerLoop polygon)
                (innerLoops polygon)
                0
                0
                0

        _ ->
            Nothing


centroidHelp :
    Float
    -> Float
    -> Point2d units coordinates
    -> List (Point2d units coordinates)
    -> List (List (Point2d units coordinates))
    -> Float
    -> Float
    -> Float
    -> Maybe (Point2d units coordinates)
centroidHelp x0 y0 firstPoint currentLoop remainingLoops xSum ySum areaSum =
    case currentLoop of
        [] ->
            case remainingLoops of
                loop :: newRemainingLoops ->
                    case loop of
                        first :: _ :: _ ->
                            -- enqueue a new loop
                            centroidHelp
                                x0
                                y0
                                first
                                loop
                                newRemainingLoops
                                xSum
                                ySum
                                areaSum

                        _ ->
                            -- skip a loop with < 2 points
                            centroidHelp
                                x0
                                y0
                                firstPoint
                                []
                                newRemainingLoops
                                xSum
                                ySum
                                areaSum

                [] ->
                    if areaSum > 0 then
                        Just
                            (Point2d.unsafe
                                { x = xSum / (areaSum * 3) + x0
                                , y = ySum / (areaSum * 3) + y0
                                }
                            )

                    else
                        Nothing

        point1 :: currentLoopRest ->
            case currentLoopRest of
                point2 :: _ ->
                    let
                        p1 =
                            Point2d.unwrap point1

                        p2 =
                            Point2d.unwrap point2

                        p1x =
                            p1.x - x0

                        p1y =
                            p1.y - y0

                        p2x =
                            p2.x - x0

                        p2y =
                            p2.y - y0

                        a =
                            p1x * p2y - p2x * p1y

                        newXSum =
                            xSum + (p1x + p2x) * a

                        newYSum =
                            ySum + (p1y + p2y) * a

                        newAreaSum =
                            areaSum + a
                    in
                    centroidHelp
                        x0
                        y0
                        firstPoint
                        currentLoopRest
                        remainingLoops
                        newXSum
                        newYSum
                        newAreaSum

                [] ->
                    let
                        p1 =
                            Point2d.unwrap point1

                        p2 =
                            Point2d.unwrap firstPoint

                        p1x =
                            p1.x - x0

                        p1y =
                            p1.y - y0

                        p2x =
                            p2.x - x0

                        p2y =
                            p2.y - y0

                        a =
                            p1x * p2y - p2x * p1y

                        newXSum =
                            xSum + (p1x + p2x) * a

                        newYSum =
                            ySum + (p1y + p2y) * a

                        newAreaSum =
                            areaSum + a
                    in
                    case remainingLoops of
                        loop :: newRemainingLoops ->
                            case loop of
                                first :: _ :: _ ->
                                    -- enqueue a new loop
                                    centroidHelp
                                        x0
                                        y0
                                        first
                                        loop
                                        newRemainingLoops
                                        newXSum
                                        newYSum
                                        newAreaSum

                                _ ->
                                    -- skip a loop with < 2 points
                                    centroidHelp
                                        x0
                                        y0
                                        firstPoint
                                        []
                                        newRemainingLoops
                                        newXSum
                                        newYSum
                                        newAreaSum

                        [] ->
                            if newAreaSum > 0 then
                                Just
                                    (Point2d.unsafe
                                        { x = newXSum / (newAreaSum * 3) + x0
                                        , y = newYSum / (newAreaSum * 3) + y0
                                        }
                                    )

                            else
                                Nothing


{-| Scale a polygon about a given center point by a given scale. If the given
scale is negative, the order of the polygon's vertices will be reversed so that
the resulting polygon still has its outer vertices in counterclockwise order and
its inner vertices in clockwise order.
-}
scaleAbout : Point2d units coordinates -> Float -> Polygon2d units coordinates -> Polygon2d units coordinates
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale) (scale < 0)


{-| Rotate a polygon around the given center point counterclockwise by the given
angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Polygon2d units coordinates -> Polygon2d units coordinates
rotateAround point angle =
    mapVertices (Point2d.rotateAround point angle) False


{-| Translate a polygon by the given displacement.
-}
translateBy : Vector2d units coordinates -> Polygon2d units coordinates -> Polygon2d units coordinates
translateBy vector =
    mapVertices (Point2d.translateBy vector) False


{-| Translate a polygon in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Polygon2d units coordinates -> Polygon2d units coordinates
translateIn direction distance polygon =
    translateBy (Vector2d.withLength distance direction) polygon


{-| Mirror a polygon across the given axis. The order of the polygon's vertices
will be reversed so that the resulting polygon still has its outer vertices in
counterclockwise order and its inner vertices in clockwise order.
-}
mirrorAcross : Axis2d units coordinates -> Polygon2d units coordinates -> Polygon2d units coordinates
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis) True


mapVertices : (Point2d unitsA coordinatesA -> Point2d unitsB coordinatesB) -> Bool -> Polygon2d unitsA coordinatesA -> Polygon2d unitsB coordinatesB
mapVertices function invert polygon =
    let
        mappedOuterLoop =
            List.map function (outerLoop polygon)

        mappedInnerLoops =
            List.map (List.map function) (innerLoops polygon)
    in
    if invert then
        Types.Polygon2d
            { outerLoop = List.reverse mappedOuterLoop
            , innerLoops = List.map List.reverse mappedInnerLoops
            }

    else
        Types.Polygon2d
            { outerLoop = mappedOuterLoop
            , innerLoops = mappedInnerLoops
            }


{-| Take a polygon defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame. If the given frame is
left-handed, the order of the polygon's vertices will be reversed so that the
resulting polygon still has its outer vertices in counterclockwise order and its
inner vertices in clockwise order.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Polygon2d units globalCoordinates -> Polygon2d units localCoordinates
relativeTo frame =
    mapVertices (Point2d.relativeTo frame) (not (Frame2d.isRightHanded frame))


{-| Take a polygon considered to be defined in local coordinates relative to a
given reference frame, and return that polygon expressed in global coordinates.
If the given frame is left-handed, the order of the polygon's vertices will be
reversed so that the resulting polygon still has its outer vertices in
counterclockwise order and its inner vertices in clockwise order.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Polygon2d units localCoordinates -> Polygon2d units globalCoordinates
placeIn frame =
    mapVertices (Point2d.placeIn frame) (not (Frame2d.isRightHanded frame))


{-| Get the minimal bounding box containing a given polygon. Returns `Nothing`
if the polygon has no vertices.

    Polygon2d.boundingBox rectangle
    --> Just <|
    -->     BoundingBox2d.from
    -->         (Point2d.meters 1 1)
    -->         (Point2d.meters 3 2)

-}
boundingBox : Polygon2d units coordinates -> Maybe (BoundingBox2d units coordinates)
boundingBox polygon =
    BoundingBox2d.hullN (outerLoop polygon)


{-| Triangulate a polygon. This uses the `TriangularMesh` data types from
[`ianmackenzie/elm-triangular-mesh`](http://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest).
Triangulation is useful for things like WebGL rendering; you can define a
polygon just by specifying its outline (and holes, if it has any)

![Polygon with hole](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/triangulate1.svg)

then use this function to turn that polygon into a list of triangles which you
can render using WebGL:

![Polygon with hole](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/triangulate2.svg)

-}
triangulate : Polygon2d units coordinates -> TriangularMesh (Point2d units coordinates)
triangulate polygon =
    Monotone.triangulation polygon


{-| Triangulate a polygon with additional control over the size of the triangles
in the end result. For example, to triangulate a given polygon while making sure
that every resulting triangle edge was no more than 10 centimeters long, you
could use:

    Polygon2d.triangulateWith
        (Polygon2d.maxEdgeLength (Length.centimeters 10))
        polygon

Note that this means that existing polygon edges may be split into smaller ones,
and new vertices will likely end up being added inside the polygon. Also note
that the resulting triangles may still be very thin; no particular attempt is
made to avoid large or small angles in triangles.

-}
triangulateWith : TriangulationRule units coordinates -> Polygon2d units coordinates -> TriangularMesh (Point2d units coordinates)
triangulateWith triangulationRule polygon =
    let
        (TriangulationRule subdivisionFunction) =
            triangulationRule
    in
    Monotone.triangulation polygon
        |> Refinement.refine subdivisionFunction


{-| A `TriangulationRule` controls the polygon triangulation process and
determines how large the faces/edges are in the resulting triangular mesh.
-}
type TriangulationRule units coordinates
    = TriangulationRule (Point2d units coordinates -> Point2d units coordinates -> Int)


{-| Ensure that every edge in a triangulation has at most the given length.
-}
maxEdgeLength : Quantity Float units -> TriangulationRule units coordinates
maxEdgeLength givenLength =
    TriangulationRule <|
        if givenLength |> Quantity.lessThanOrEqualTo Quantity.zero then
            \_ _ -> 0

        else
            \startPoint endPoint ->
                let
                    distance =
                        Point2d.distanceFrom startPoint endPoint
                in
                ceiling (Quantity.ratio distance givenLength)


{-| Ensure that every triangle in a triangulation has at most the given width
and the given height.
-}
maxTriangleDimensions : Quantity Float units -> Quantity Float units -> TriangulationRule units coordinates
maxTriangleDimensions width height =
    TriangulationRule <|
        if
            (width |> Quantity.lessThanOrEqualTo Quantity.zero)
                || (height |> Quantity.lessThanOrEqualTo Quantity.zero)
        then
            \_ _ -> 0

        else
            let
                (Quantity w) =
                    width

                (Quantity h) =
                    height
            in
            \(Types.Point2d p1) (Types.Point2d p2) ->
                ceiling <|
                    max
                        (abs (p2.x - p1.x) / w)
                        (abs (p2.y - p1.y) / h)


{-| Check if a polygon contains a given point.

This is an O(n) operation. The polygon can have holes and does not need to be convex.

-}
contains : Point2d units coordinates -> Polygon2d units coordinates -> Bool
contains point polygon =
    let
        (Quantity x) =
            Point2d.xCoordinate point

        (Quantity y) =
            Point2d.yCoordinate point
    in
    containsPointHelp (edges polygon) x y 0


containsPointHelp : List (LineSegment2d units coordinates) -> Float -> Float -> Int -> Bool
containsPointHelp edgeList xp yp k =
    -- Based on Hao, J.; Sun, J.; Chen, Y.; Cai, Q.; Tan, L. Optimal Reliable Point-in-Polygon Test and
    -- Differential Coding Boolean Operations on Polygons. Symmetry 2018, 10, 477.
    -- https://www.mdpi.com/2073-8994/10/10/477/pdf
    case edgeList of
        [] ->
            not (modBy 2 k == 0)

        edge :: rest ->
            let
                ( p0, p1 ) =
                    LineSegment2d.endpoints edge

                (Quantity xi) =
                    Point2d.xCoordinate p0

                (Quantity yi) =
                    Point2d.yCoordinate p0

                (Quantity xi1) =
                    Point2d.xCoordinate p1

                (Quantity yi1) =
                    Point2d.yCoordinate p1

                v1 =
                    yi - yp

                v2 =
                    yi1 - yp
            in
            if (v1 < 0 && v2 < 0) || (v1 > 0 && v2 > 0) then
                -- case 11 or 26
                containsPointHelp rest xp yp k

            else
                let
                    u1 =
                        xi - xp

                    u2 =
                        xi1 - xp
                in
                if v2 > 0 && v1 <= 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f > 0 then
                        -- case 3 or 9
                        containsPointHelp rest xp yp (k + 1)

                    else if f == 0 then
                        -- case 16 or 21
                        True

                    else
                        -- case 13 or 24
                        containsPointHelp rest xp yp k

                else if v1 > 0 && v2 <= 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f < 0 then
                        -- case 4 or 10
                        containsPointHelp rest xp yp (k + 1)

                    else if f == 0 then
                        -- case 19 or 20
                        True

                    else
                        -- case 12 or 25
                        containsPointHelp rest xp yp k

                else if v2 == 0 && v1 < 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f == 0 then
                        -- case 17
                        True

                    else
                        -- case 7 or 14
                        containsPointHelp rest xp yp k

                else if v1 == 0 && v2 < 0 then
                    let
                        f =
                            u1 * v2 - u2 * v1
                    in
                    if f == 0 then
                        -- case 18
                        True

                    else
                        -- case 8 or 15
                        containsPointHelp rest xp yp k

                else if v1 == 0 && v2 == 0 then
                    if u2 <= 0 && u1 >= 0 then
                        -- case 1
                        True

                    else if u1 <= 0 && u2 >= 0 then
                        -- case 2
                        True

                    else
                        --  case 5, 6, 22, 23
                        containsPointHelp rest xp yp k

                else
                    containsPointHelp rest xp yp k
