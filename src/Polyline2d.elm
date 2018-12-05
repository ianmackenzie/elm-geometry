--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polyline2d exposing
    ( Polyline2d
    , fromVertices
    , vertices, segments, length, boundingBox
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices
    , relativeTo, placeIn
    , centroid
    )

{-| A `Polyline2d` represents a sequence of vertices in 2D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline2d


# Constructors

@docs fromVertices


# Properties

@docs vertices, segments, length, boundingBox


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


{-| -}
type alias Polyline2d units coordinates =
    Types.Polyline2d units coordinates


{-| Construct a polyline from a list of vertices:

    stepShape =
        Polyline2d.fromVertices
            [ Point2d.fromCoordinates ( 0, 0 )
            , Point2d.fromCoordinates ( 1, 0 )
            , Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 2, 1 )
            ]

-}
fromVertices : List (Point2d units coordinates) -> Polyline2d units coordinates
fromVertices givenVertices =
    Types.Polyline2d givenVertices


{-| Get the vertices of a polyline.

    Polyline2d.vertices stepShape
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 1, 0 )
    --> , Point2d.fromCoordinates ( 1, 1 )
    --> , Point2d.fromCoordinates ( 2, 1 )
    --> ]

-}
vertices : Polyline2d units coordinates -> List (Point2d units coordinates)
vertices (Types.Polyline2d polylineVertices) =
    polylineVertices


{-| Get the individual segments of a polyline.

    Polyline2d.segments stepShape
    --> [ LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     )
    --> , LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 1, 1 )
    -->     )
    --> , LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 1 )
    -->     , Point2d.fromCoordinates ( 2, 1 )
    -->     )
    --> ]

-}
segments : Polyline2d units coordinates -> List (LineSegment2d units coordinates)
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment2d.from all rest


{-| Get the overall length of a polyline (the sum of the lengths of its
segments).

    Polyline2d.length stepShape
    --> 3

-}
length : Polyline2d units coordinates -> Quantity Float units
length polyline =
    segments polyline |> List.map LineSegment2d.length |> Quantity.sum


{-| Scale a polyline about a given center point by a given scale.

    point =
        Point2d.fromCoordinates ( 1, 0 )

    Polyline2d.scaleAbout point 2 stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( -1, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 1, 2 )
    -->     , Point2d.fromCoordinates ( 3, 2 )
    -->     ]

-}
scaleAbout : Point2d units coordinates -> Float -> Polyline2d units coordinates -> Polyline2d units coordinates
scaleAbout point scale polyline =
    mapVertices (Point2d.scaleAbout point scale) polyline


{-| Rotate a polyline around the given center point counterclockwise by the
given angle (in radians).

    stepShape
        |> Polyline2d.rotateAround Point2d.origin
            (degrees 90)
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 0, 1 )
    -->     , Point2d.fromCoordinates ( -1, 1 )
    -->     , Point2d.fromCoordinates ( -1, 2 )
    -->     ]

-}
rotateAround : Point2d units coordinates -> Angle -> Polyline2d units coordinates -> Polyline2d units coordinates
rotateAround point angle polyline =
    mapVertices (Point2d.rotateAround point angle) polyline


{-| Translate a polyline by the given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Polyline2d.translateBy displacement stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 3, 3 )
    -->     , Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 4, 4 )
    -->     ]

-}
translateBy : Vector2d units coordinates -> Polyline2d units coordinates -> Polyline2d units coordinates
translateBy vector polyline =
    mapVertices (Point2d.translateBy vector) polyline


{-| Translate a polyline in a given direction by a given distance;

    Polyline2d.translateIn direction distance

is equivalent to

    Polyline2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d coordinates -> Quantity Float units -> Polyline2d units coordinates -> Polyline2d units coordinates
translateIn direction distance polyline =
    translateBy (Vector2d.withLength distance direction) polyline


{-| Mirror a polyline across the given axis.

    Polyline2d.mirrorAcross Axis2d.x stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 1, -1 )
    -->     , Point2d.fromCoordinates ( 2, -1 )
    -->     ]

-}
mirrorAcross : Axis2d units coordinates -> Polyline2d units coordinates -> Polyline2d units coordinates
mirrorAcross axis polyline =
    mapVertices (Point2d.mirrorAcross axis) polyline


{-| Project (flatten) a polyline onto the given axis.

    Polyline2d.projectOnto Axis2d.x stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 2, 0 )
    -->     ]

-}
projectOnto : Axis2d units coordinates -> Polyline2d units coordinates -> Polyline2d units coordinates
projectOnto axis polyline =
    mapVertices (Point2d.projectOnto axis) polyline


{-| Transform each vertex of a polyline by the given function. All other
transformations can be defined in terms of `mapVertices`; for example,

    Polyline2d.mirrorAcross axis

is equivalent to

    Polyline2d.mapVertices (Point2d.mirrorAcross axis)

-}
mapVertices : (Point2d units1 coordinates1 -> Point2d units2 coordinates2) -> Polyline2d units1 coordinates1 -> Polyline2d units2 coordinates2
mapVertices function polyline =
    vertices polyline |> List.map function |> fromVertices


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Polyline2d.relativeTo localFrame stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( -1, -2 )
    -->     , Point2d.fromCoordinates ( 0, -2 )
    -->     , Point2d.fromCoordinates ( 0, -1 )
    -->     , Point2d.fromCoordinates ( 1, -1 )
    -->     ]

-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Polyline2d units globalCoordinates -> Polyline2d units localCoordinates
relativeTo frame polyline =
    mapVertices (Point2d.relativeTo frame) polyline


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Polyline2d.placeIn localFrame stepShape
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 1, 2 )
    -->     , Point2d.fromCoordinates ( 2, 2 )
    -->     , Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 3, 3 )
    -->     ]

-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Polyline2d units localCoordinates -> Polyline2d units globalCoordinates
placeIn frame polyline =
    mapVertices (Point2d.placeIn frame) polyline


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline2d.boundingBox stepShape
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 0
    -->         , maxX = 2
    -->         , minY = 0
    -->         , maxY = 1
    -->         }
    -->     )

-}
boundingBox : Polyline2d units coordinates -> Maybe (BoundingBox2d units coordinates)
boundingBox polyline =
    BoundingBox2d.containingPoints (vertices polyline)


{-| Find the centroid of the polyline. Returns `Nothing` if the polyline
has no vertices.

    Polyline2d.centroid stepShape
    --> Just (Point2d.fromCoordinates ( 1.0, 0.5 ))

-}
centroid : Polyline2d units coordinates -> Maybe (Point2d units coordinates)
centroid polyline =
    case ( vertices polyline, boundingBox polyline ) of
        ( [], _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( first :: _, Just box ) ->
            let
                polylineLength =
                    length polyline
            in
            if polylineLength == Quantity.zero then
                Just first

            else
                let
                    roughCentroid =
                        BoundingBox2d.centerPoint box

                    helper =
                        refineBySegment polylineLength roughCentroid
                in
                segments polyline
                    |> List.foldl helper roughCentroid
                    |> Just


refineBySegment : Quantity Float units -> Point2d units coordinates -> LineSegment2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
refineBySegment polylineLength roughCentroid segment currentCentroid =
    let
        segmentMidpoint =
            LineSegment2d.midpoint segment

        segmentLength =
            LineSegment2d.length segment
    in
    Vector2d.from roughCentroid segmentMidpoint
        |> Vector2d.scaleBy (Quantity.ratio segmentLength polylineLength)
        |> (\v -> Point2d.translateBy v currentCentroid)
