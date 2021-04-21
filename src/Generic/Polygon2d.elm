module Generic.Polygon2d exposing
    ( Polygon2d
    , singleLoop, singleLoopBy, withHoles, withHolesBy, convexHull, convexHullBy
    , outerLoop, innerLoops, vertices, edges, edgesBy, edgesOf, perimeter, perimeterBy, area, areaBy, centroid, centroidBy, boundingBox, boundingBoxBy
    , contains, containsBy
    , mapVertices
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

@docs singleLoop, singleLoopBy, withHoles, withHolesBy, convexHull, convexHullBy


# Properties

@docs outerLoop, innerLoops, vertices, edges, edgesBy, edgesOf, perimeter, perimeterBy, area, areaBy, centroid, centroidBy, boundingBox, boundingBoxBy


# Queries

@docs contains, containsBy


# Mapping

@docs mapVertices


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
type alias Polygon2d vertex =
    Types.Polygon vertex


counterclockwiseArea : (vertex -> Point2d units coordinates) -> List vertex -> Quantity Float (Squared units)
counterclockwiseArea getPosition vertices_ =
    case vertices_ of
        [] ->
            Quantity.zero

        [ single ] ->
            Quantity.zero

        [ first, second ] ->
            Quantity.zero

        first :: second :: rest ->
            let
                firstPosition =
                    getPosition first

                segmentArea start end =
                    Triangle2d.counterclockwiseArea
                        (Triangle2d.from firstPosition (getPosition start) (getPosition end))

                segmentAreas =
                    List.map2 segmentArea (second :: rest) rest
            in
            Quantity.sum segmentAreas


makeOuterLoop : (vertex -> Point2d units coordinates) -> List vertex -> List vertex
makeOuterLoop getPosition vertices_ =
    if
        counterclockwiseArea getPosition vertices_
            |> Quantity.greaterThanOrEqualTo Quantity.zero
    then
        vertices_

    else
        List.reverse vertices_


makeInnerLoop : (vertex -> Point2d units coordinates) -> List vertex -> List vertex
makeInnerLoop getPosition vertices_ =
    if
        counterclockwiseArea getPosition vertices_
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
singleLoop : List (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
singleLoop givenOuterLoop =
    singleLoopBy identity givenOuterLoop


singleLoopBy : (vertex -> Point2d units coordinates) -> List vertex -> Polygon2d vertex
singleLoopBy getPosition givenOuterLoop =
    withHolesBy getPosition [] givenOuterLoop


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
withHoles : List (List (Point2d units coordinates)) -> List (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
withHoles givenInnerLoops givenOuterLoop =
    withHolesBy identity givenInnerLoops givenOuterLoop


withHolesBy : (vertex -> Point2d units coordinates) -> List (List vertex) -> List vertex -> Polygon2d vertex
withHolesBy getPosition givenInnerLoops givenOuterLoop =
    Types.Polygon2d
        { outerLoop = makeOuterLoop getPosition givenOuterLoop
        , innerLoops = List.map (makeInnerLoop getPosition) givenInnerLoops
        }


counterclockwiseAround : (vertex -> Point2d units coordinates) -> vertex -> vertex -> vertex -> Bool
counterclockwiseAround getPosition origin a b =
    let
        originPoint =
            getPosition origin

        crossProduct =
            Vector2d.from originPoint (getPosition a)
                |> Vector2d.cross
                    (Vector2d.from originPoint (getPosition b))
    in
    crossProduct |> Quantity.greaterThanOrEqualTo Quantity.zero


chainHelp : (vertex -> Point2d units coordinates) -> List vertex -> List vertex -> List vertex
chainHelp getPosition acc list =
    case ( acc, list ) of
        ( r1 :: r2 :: rs, x :: xs ) ->
            if counterclockwiseAround getPosition r2 r1 x then
                chainHelp getPosition (r2 :: rs) (x :: xs)

            else
                chainHelp getPosition (x :: acc) xs

        ( _, x :: xs ) ->
            chainHelp getPosition (x :: acc) xs

        ( _, [] ) ->
            List.drop 1 acc


chain : (vertex -> Point2d units coordinates) -> List vertex -> List vertex
chain getPosition list =
    chainHelp getPosition [] list


compareCoordinates : (vertex -> Point2d units coordinates) -> vertex -> vertex -> Order
compareCoordinates getPosition firstVertex secondVertex =
    let
        firstPoint =
            getPosition firstVertex

        secondPoint =
            getPosition secondVertex
    in
    if Point2d.xCoordinate firstPoint == Point2d.xCoordinate secondPoint then
        Quantity.compare (Point2d.yCoordinate firstPoint) (Point2d.yCoordinate secondPoint)

    else
        Quantity.compare (Point2d.xCoordinate firstPoint) (Point2d.xCoordinate secondPoint)


{-| Build the [convex hull](https://en.wikipedia.org/wiki/Convex_hull) of a list
of points:

![Convex hull of a set of points](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/convexHull.svg)

This is an O(n log n) operation.

-}
convexHull : List (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
convexHull points =
    convexHullBy identity points


convexHullBy : (vertex -> Point2d units coordinates) -> List vertex -> Polygon2d vertex
convexHullBy getPosition givenVertices =
    -- See http://www.algorithmist.com/index.php/Monotone_Chain_Convex_Hull
    -- for a description of the algorithm.
    let
        sorted =
            givenVertices |> List.sortWith (compareCoordinates getPosition)

        lower =
            chain getPosition sorted

        upper =
            chain getPosition (List.reverse sorted)
    in
    singleLoopBy getPosition (lower ++ upper)


{-| Convert a polygon from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Polygon2d (Point2d units1 coordinates) -> Polygon2d (Point2d units2 coordinates)
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
at_ : Quantity Float (Rate units1 units2) -> Polygon2d (Point2d units1 coordinates) -> Polygon2d (Point2d units2 coordinates)
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
outerLoop : Polygon2d vertex -> List vertex
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
innerLoops : Polygon2d vertex -> List (List vertex)
innerLoops (Types.Polygon2d polygon) =
    polygon.innerLoops


{-| Get all vertices of a polygon; this will include vertices from the outer
loop of the polygon and all inner loops. The order of the returned vertices is
undefined.
-}
vertices : Polygon2d vertex -> List vertex
vertices polygon =
    List.concat (outerLoop polygon :: innerLoops polygon)


loopEdges : (vertex -> vertex -> edge) -> List vertex -> List edge
loopEdges makeEdge vertices_ =
    case vertices_ of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 makeEdge all (rest ++ [ first ])


{-| Get all edges of a polygon. This will include both outer edges and inner
(hole) edges.
-}
edges : Polygon2d (Point2d units coordinates) -> List (LineSegment2d units coordinates)
edges polygon =
    edgesOf LineSegment2d.from polygon


edgesBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> List (LineSegment2d units coordinates)
edgesBy getPosition polygon =
    edgesOf (\v1 v2 -> LineSegment2d.from (getPosition v1) (getPosition v2)) polygon


edgesOf : (vertex -> vertex -> edge) -> Polygon2d vertex -> List edge
edgesOf makeEdge polygon =
    let
        outerEdges =
            loopEdges makeEdge (outerLoop polygon)

        innerEdges =
            List.map (loopEdges makeEdge) (innerLoops polygon)
    in
    List.concat (outerEdges :: innerEdges)


{-| Get the perimeter of a polygon (the sum of the lengths of its edges). This
includes the outer perimeter and the perimeter of any holes.

    Polygon2d.perimeter squareWithHole
    --> Length.meters 16

-}
perimeter : Polygon2d (Point2d units coordinates) -> Quantity Float units
perimeter polygon =
    edges polygon |> List.map LineSegment2d.length |> Quantity.sum


perimeterBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> Quantity Float units
perimeterBy getPosition polygon =
    Quantity.sum <|
        edgesOf (\v1 v2 -> Point2d.distanceFrom (getPosition v1) (getPosition v2)) polygon


{-| Get the area of a polygon. This value will never be negative.

    Polygon2d.area squareWithHole
    --> Area.squareMeters 8

-}
area : Polygon2d (Point2d units coordinates) -> Quantity Float (Squared units)
area polygon =
    areaBy identity polygon


areaBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> Quantity Float (Squared units)
areaBy getPosition polygon =
    counterclockwiseArea getPosition (outerLoop polygon)
        |> Quantity.plus
            (Quantity.sum (List.map (counterclockwiseArea getPosition) (innerLoops polygon)))


{-| Get the centroid of a polygon. Returns `Nothing` if the polygon has no
vertices or zero area.
-}
centroid : Polygon2d (Point2d units coordinates) -> Maybe (Point2d units coordinates)
centroid polygon =
    centroidBy identity polygon


centroidBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> Maybe (Point2d units coordinates)
centroidBy getPosition polygon =
    case outerLoop polygon of
        first :: _ :: _ ->
            let
                firstPosition =
                    getPosition first

                offset =
                    Point2d.unwrap firstPosition
            in
            centroidHelp
                getPosition
                offset.x
                offset.y
                firstPosition
                (outerLoop polygon)
                (innerLoops polygon)
                0
                0
                0

        _ ->
            Nothing


centroidHelp :
    (vertex -> Point2d units coordinates)
    -> Float
    -> Float
    -> Point2d units coordinates
    -> List vertex
    -> List (List vertex)
    -> Float
    -> Float
    -> Float
    -> Maybe (Point2d units coordinates)
centroidHelp getPosition x0 y0 firstPoint currentLoop remainingLoops xSum ySum areaSum =
    case currentLoop of
        [] ->
            case remainingLoops of
                loop :: newRemainingLoops ->
                    case loop of
                        first :: _ :: _ ->
                            -- enqueue a new loop
                            centroidHelp
                                getPosition
                                x0
                                y0
                                (getPosition first)
                                loop
                                newRemainingLoops
                                xSum
                                ySum
                                areaSum

                        _ ->
                            -- skip a loop with < 2 points
                            centroidHelp
                                getPosition
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

        vertex1 :: currentLoopRest ->
            case currentLoopRest of
                vertex2 :: _ ->
                    let
                        p1 =
                            Point2d.unwrap (getPosition vertex1)

                        p2 =
                            Point2d.unwrap (getPosition vertex2)

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
                        getPosition
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
                            Point2d.unwrap (getPosition vertex1)

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
                                        getPosition
                                        x0
                                        y0
                                        (getPosition first)
                                        loop
                                        newRemainingLoops
                                        newXSum
                                        newYSum
                                        newAreaSum

                                _ ->
                                    -- skip a loop with < 2 points
                                    centroidHelp
                                        getPosition
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


mapVertices : (vertex1 -> vertex2) -> (vertex2 -> Point2d units coordinates) -> Polygon2d vertex1 -> Polygon2d vertex2
mapVertices function getPosition polygon =
    let
        mappedOuterLoop =
            List.map function (outerLoop polygon)

        mappedInnerLoops =
            List.map (List.map function) (innerLoops polygon)
    in
    withHolesBy getPosition mappedInnerLoops mappedOuterLoop


{-| Scale a polygon about a given center point by a given scale. If the given
scale is negative, the order of the polygon's vertices will be reversed so that
the resulting polygon still has its outer vertices in counterclockwise order and
its inner vertices in clockwise order.
-}
scaleAbout : Point2d units coordinates -> Float -> Polygon2d (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
scaleAbout point scale =
    fastMap (scale >= 0) (Point2d.scaleAbout point scale)


{-| Rotate a polygon around the given center point counterclockwise by the given
angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Polygon2d (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
rotateAround point angle =
    fastMap True (Point2d.rotateAround point angle)


{-| Translate a polygon by the given displacement.
-}
translateBy : Vector2d units coordinates -> Polygon2d (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
translateBy vector =
    fastMap True (Point2d.translateBy vector)


{-| Translate a polygon in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Polygon2d (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
translateIn direction distance polygon =
    translateBy (Vector2d.withLength distance direction) polygon


{-| Mirror a polygon across the given axis. The order of the polygon's vertices
will be reversed so that the resulting polygon still has its outer vertices in
counterclockwise order and its inner vertices in clockwise order.
-}
mirrorAcross : Axis2d units coordinates -> Polygon2d (Point2d units coordinates) -> Polygon2d (Point2d units coordinates)
mirrorAcross axis =
    fastMap False (Point2d.mirrorAcross axis)


fastMap :
    Bool
    -> (Point2d unitsA coordinatesA -> Point2d unitsB coordinatesB)
    -> Polygon2d (Point2d unitsA coordinatesA)
    -> Polygon2d (Point2d unitsB coordinatesB)
fastMap signPreserving function polygon =
    let
        mappedOuterLoop =
            List.map function (outerLoop polygon)

        mappedInnerLoops =
            List.map (List.map function) (innerLoops polygon)
    in
    if signPreserving then
        Types.Polygon2d
            { outerLoop = mappedOuterLoop
            , innerLoops = mappedInnerLoops
            }

    else
        Types.Polygon2d
            { outerLoop = List.reverse mappedOuterLoop
            , innerLoops = List.map List.reverse mappedInnerLoops
            }


{-| Take a polygon defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame. If the given frame is
left-handed, the order of the polygon's vertices will be reversed so that the
resulting polygon still has its outer vertices in counterclockwise order and its
inner vertices in clockwise order.
-}
relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Polygon2d (Point2d units globalCoordinates)
    -> Polygon2d (Point2d units localCoordinates)
relativeTo frame =
    fastMap (Frame2d.isRightHanded frame) (Point2d.relativeTo frame)


{-| Take a polygon considered to be defined in local coordinates relative to a
given reference frame, and return that polygon expressed in global coordinates.
If the given frame is left-handed, the order of the polygon's vertices will be
reversed so that the resulting polygon still has its outer vertices in
counterclockwise order and its inner vertices in clockwise order.
-}
placeIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Polygon2d (Point2d units localCoordinates)
    -> Polygon2d (Point2d units globalCoordinates)
placeIn frame =
    fastMap (Frame2d.isRightHanded frame) (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given polygon. Returns `Nothing`
if the polygon has no vertices.

    Polygon2d.boundingBox rectangle
    --> Just <|
    -->     BoundingBox2d.from
    -->         (Point2d.meters 1 1)
    -->         (Point2d.meters 3 2)

-}
boundingBox : Polygon2d (Point2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
boundingBox polygon =
    BoundingBox2d.hullN (outerLoop polygon)


boundingBoxBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> Maybe (BoundingBox2d units coordinates)
boundingBoxBy getPosition polygon =
    BoundingBox2d.hullOfN getPosition (outerLoop polygon)


{-| Triangulate a polygon. This uses the `TriangularMesh` data types from
[`ianmackenzie/elm-triangular-mesh`](http://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest).
Triangulation is useful for things like WebGL rendering; you can define a
polygon just by specifying its outline (and holes, if it has any)

![Polygon with hole](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/triangulate1.svg)

then use this function to turn that polygon into a list of triangles which you
can render using WebGL:

![Polygon with hole](https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/triangulate2.svg)

-}
triangulate : Polygon2d (Point2d units coordinates) -> TriangularMesh (Point2d units coordinates)
triangulate polygon =
    triangulateBy identity polygon


triangulateBy : (vertex -> Point2d units coordinates) -> Polygon2d vertex -> TriangularMesh vertex
triangulateBy getPosition polygon =
    Monotone.triangulation getPosition polygon


{-| -}
type alias TriangulationConfig vertex units coordinates =
    { triangulationRule : TriangulationRule units coordinates
    , vertexPosition : vertex -> Point2d units coordinates
    , midpoint : vertex -> vertex -> vertex
    }


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
triangulateWith : TriangulationConfig vertex units coordinates -> Polygon2d vertex -> TriangularMesh vertex
triangulateWith config polygon =
    let
        initialTriangulation =
            Monotone.triangulation config.vertexPosition polygon
    in
    case config.triangulationRule of
        NoRestrictions ->
            initialTriangulation

        TriangulationRule subdivisionFunction ->
            initialTriangulation
                |> Refinement.refine
                    { subdivisionFunction = subdivisionFunction
                    , vertexPosition = config.vertexPosition
                    , midpoint = config.midpoint
                    }


{-| A `TriangulationRule` controls the polygon triangulation process and
determines how large the faces/edges are in the resulting triangular mesh.
-}
type TriangulationRule units coordinates
    = NoRestrictions
    | TriangulationRule (Point2d units coordinates -> Point2d units coordinates -> Int)


noRestrictions : TriangulationRule units coordinates
noRestrictions =
    NoRestrictions


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
contains : Point2d units coordinates -> Polygon2d (Point2d units coordinates) -> Bool
contains point polygon =
    let
        { x, y } =
            Point2d.unwrap point
    in
    containsPointHelp (edges polygon) x y 0


containsBy : (vertex -> Point2d units coordinates) -> Point2d units coordinates -> Polygon2d vertex -> Bool
containsBy getPosition point polygon =
    let
        { x, y } =
            Point2d.unwrap point
    in
    containsPointHelp (edgesBy getPosition polygon) x y 0


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
