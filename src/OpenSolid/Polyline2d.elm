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


module OpenSolid.Polyline2d
    exposing
        ( Polyline2d
        , boundingBox
        , length
        , map
        , mirrorAcross
        , placeIn
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , segments
        , translateBy
        , vertices
        , withVertices
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/polyline2d.svg" alt="Polyline2d" width="160">

A `Polyline2d` represents a sequence of vertices in 2D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline2d


# Constructors

@docs withVertices


# Accessors

@docs vertices, segments


# Length

@docs length


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, map


# Coordinate frames

@docs relativeTo, placeIn


# Bounds

@docs boundingBox

-}

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias Polyline2d =
    Internal.Polyline2d


{-| Construct a polyline from a list of vertices:

    stepShape =
        Polyline2d.withVertices
            [ Point2d.withCoordinates ( 0, 0 )
            , Point2d.withCoordinates ( 1, 0 )
            , Point2d.withCoordinates ( 1, 1 )
            , Point2d.withCoordinates ( 2, 1 )
            ]

-}
withVertices : List Point2d -> Polyline2d
withVertices =
    Internal.Polyline2d


{-| Get the vertices of a polyline.

    Polyline2d.vertices stepShape
    --> [ Point2d.withCoordinates ( 0, 0 )
    --> , Point2d.withCoordinates ( 1, 0 )
    --> , Point2d.withCoordinates ( 1, 1 )
    --> , Point2d.withCoordinates ( 2, 1 )
    --> ]

-}
vertices : Polyline2d -> List Point2d
vertices (Internal.Polyline2d vertices_) =
    vertices_


{-| Get the individual segments of a polyline.

    Polyline2d.segments stepShape
    --> [ LineSegment2d.withEndpoints
    -->     ( Point2d.withCoordinates ( 0, 0 )
    -->     , Point2d.withCoordinates ( 1, 0 )
    -->     )
    --> , LineSegment2d.withEndpoints
    -->     ( Point2d.withCoordinates ( 1, 0 )
    -->     , Point2d.withCoordinates ( 1, 1 )
    -->     )
    --> , LineSegment2d.withEndpoints
    -->     ( Point2d.withCoordinates ( 1, 1 )
    -->     , Point2d.withCoordinates ( 2, 1 )
    -->     )
    --> ]

-}
segments : Polyline2d -> List LineSegment2d
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
length : Polyline2d -> Float
length =
    segments >> List.map LineSegment2d.length >> List.sum


{-| Scale a polyline about a given center point by a given scale.

    point =
        Point2d.withCoordinates ( 1, 0 )

    Polyline2d.scaleAbout point 2 stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( -1, 0 )
    -->     , Point2d.withCoordinates ( 1, 0 )
    -->     , Point2d.withCoordinates ( 1, 2 )
    -->     , Point2d.withCoordinates ( 3, 2 )
    -->     ]

-}
scaleAbout : Point2d -> Float -> Polyline2d -> Polyline2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


{-| Rotate a polyline around the given center point counterclockwise by the
given angle (in radians).

    Polyline2d.rotateAround Point2d.origin (degrees 90) stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( 0, 0 )
    -->     , Point2d.withCoordinates ( 0, 1 )
    -->     , Point2d.withCoordinates ( -1, 1 )
    -->     , Point2d.withCoordinates ( -1, 2 )
    -->     ]

-}
rotateAround : Point2d -> Float -> Polyline2d -> Polyline2d
rotateAround point angle =
    map (Point2d.rotateAround point angle)


{-| Translate a polyline by the given displacement.

    displacement =
        Vector2d.withComponents ( 2, 3 )

    Polyline2d.translateBy displacement stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( 2, 3 )
    -->     , Point2d.withCoordinates ( 3, 3 )
    -->     , Point2d.withCoordinates ( 3, 4 )
    -->     , Point2d.withCoordinates ( 4, 4 )
    -->     ]

-}
translateBy : Vector2d -> Polyline2d -> Polyline2d
translateBy vector =
    map (Point2d.translateBy vector)


{-| Mirror a polyline across the given axis.

    Polyline2d.mirrorAcross Axis2d.x stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( 0, 0 )
    -->     , Point2d.withCoordinates ( 1, 0 )
    -->     , Point2d.withCoordinates ( 1, -1 )
    -->     , Point2d.withCoordinates ( 2, -1 )
    -->     ]

-}
mirrorAcross : Axis2d -> Polyline2d -> Polyline2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


{-| Project (flatten) a polyline onto the given axis.

    Polyline2d.projectOnto Axis2d.x stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( 0, 0 )
    -->     , Point2d.withCoordinates ( 1, 0 )
    -->     , Point2d.withCoordinates ( 1, 0 )
    -->     , Point2d.withCoordinates ( 2, 0 )
    -->     ]

-}
projectOnto : Axis2d -> Polyline2d -> Polyline2d
projectOnto axis =
    map (Point2d.projectOnto axis)


{-| Transform each vertex of a polyline by the given function. All other
transformations can be defined in terms of `map`; for example,

    Polyline2d.mirrorAcross Axis2d.x polyline

is equivalent to

    Polyline2d.map (Point2d.mirrorAcross Axis2d.x) polyline

-}
map : (Point2d -> Point2d) -> Polyline2d -> Polyline2d
map function =
    vertices >> List.map function >> withVertices


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d.withCoordinates ( 1, 2 ))

    Polyline2d.relativeTo localFrame stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( -1, -2 )
    -->     , Point2d.withCoordinates ( 0, -2 )
    -->     , Point2d.withCoordinates ( 0, -1 )
    -->     , Point2d.withCoordinates ( 1, -1 )
    -->     ]

-}
relativeTo : Frame2d -> Polyline2d -> Polyline2d
relativeTo frame =
    map (Point2d.relativeTo frame)


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.

    localFrame =
        Frame2d.at (Point2d.withCoordinates ( 1, 2 ))

    Polyline2d.placeIn localFrame stepShape
    --> Polyline2d.withVertices
    -->     [ Point2d.withCoordinates ( 1, 2 )
    -->     , Point2d.withCoordinates ( 2, 2 )
    -->     , Point2d.withCoordinates ( 2, 3 )
    -->     , Point2d.withCoordinates ( 3, 3 )
    -->     ]

-}
placeIn : Frame2d -> Polyline2d -> Polyline2d
placeIn frame =
    map (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline2d.boundingBox stepShape
    --> Just
    -->     (BoundingBox2d.with
    -->         { minX = 0
    -->         , maxX = 2
    -->         , minY = 0
    -->         , maxY = 1
    -->         }
    -->     )

-}
boundingBox : Polyline2d -> Maybe BoundingBox2d
boundingBox polyline =
    BoundingBox2d.containing (vertices polyline)
