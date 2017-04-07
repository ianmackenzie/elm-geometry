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
        ( vertices
        , segments
        , length
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , map
        , relativeTo
        , placeIn
        , placeOnto
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/polyline2d.svg" alt="Polyline2d" width="160">

A `Polyline2d` represents a sequence of vertices connected by line segments.
This module contains a variety of polyline-related functionality, such as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

Polylines can be constructed by passing an ordered list of vertices to the
`Polyline2d` constructor, for example

    stepShape =
        Polyline2d
            [ Point2d ( 0, 0 )
            , Point2d ( 1, 0 )
            , Point2d ( 1, 1 )
            , Point2d ( 2, 1 )
            ]


# Accessors

@docs vertices, segments


# Length

@docs length


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, map


# Coordinate frames

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d


{-| Get the vertices of a polyline.

    Polyline2d.vertices stepShape
    --> [ Point2d ( 0, 0 )
    --> , Point2d ( 1, 0 )
    --> , Point2d ( 1, 1 )
    --> , Point2d ( 2, 1 )
    --> ]

-}
vertices : Polyline2d -> List Point2d
vertices (Polyline2d vertices_) =
    vertices_


{-| Get the individual segments of a polyline.

    Polyline2d.segments stepShape
    --> [ LineSegment2d ( Point2d ( 0, 0 ), Point2d ( 1, 0 ) )
    --> , LineSegment2d ( Point2d ( 1, 0 ), Point2d ( 1, 1 ) )
    --> , LineSegment2d ( Point2d ( 1, 1 ), Point2d ( 2, 1 ) )
    --> ]

-}
segments : Polyline2d -> List LineSegment2d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment2d ( start, end )) all rest


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
        Point2d ( 1, 0 )

    Polyline2d.scaleAbout point 2 stepShape
    --> Polyline2d
    -->     [ Point2d ( -1, 0 )
    -->     , Point2d ( 1, 0 )
    -->     , Point2d ( 1, 2 )
    -->     , Point2d ( 3, 2 )
    -->     ]

-}
scaleAbout : Point2d -> Float -> Polyline2d -> Polyline2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


{-| Rotate a polyline around the given center point counterclockwise by the
given angle (in radians).

    Polyline2d.rotateAround Point2d.origin (degrees 90) stepShape
    --> Polyline2d
    -->     [ Point2d ( 0, 0 )
    -->     , Point2d ( 0, 1 )
    -->     , Point2d ( -1, 1 )
    -->     , Point2d ( -1, 2 )
    -->     ]

-}
rotateAround : Point2d -> Float -> Polyline2d -> Polyline2d
rotateAround point angle =
    map (Point2d.rotateAround point angle)


{-| Translate a polyline by the given displacement.

    displacement =
        Vector2d ( 2, 3 )

    Polyline2d.translateBy displacement stepShape
    --> Polyline2d
    -->     [ Point2d ( 2, 3 )
    -->     , Point2d ( 3, 3 )
    -->     , Point2d ( 3, 4 )
    -->     , Point2d ( 4, 4 )
    -->     ]

-}
translateBy : Vector2d -> Polyline2d -> Polyline2d
translateBy vector =
    map (Point2d.translateBy vector)


{-| Mirror a polyline across the given axis.

    Polyline2d.mirrorAcross Axis2d.x stepShape
    --> Polyline2d
    -->     [ Point2d ( 0, 0 )
    -->     , Point2d ( 1, 0 )
    -->     , Point2d ( 1, -1 )
    -->     , Point2d ( 2, -1 )
    -->     ]

-}
mirrorAcross : Axis2d -> Polyline2d -> Polyline2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


{-| Project (flatten) a polyline onto the given axis.

    Polyline2d.projectOnto Axis2d.x stepShape
    --> Polyline2d
    -->     [ Point2d ( 0, 0 )
    -->     , Point2d ( 1, 0 )
    -->     , Point2d ( 1, 0 )
    -->     , Point2d ( 2, 0 )
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
    vertices >> List.map function >> Polyline2d


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Polyline2d.relativeTo localFrame stepShape
    --> Polyline2d
    -->     [ Point2d ( -1, -2 )
    -->     , Point2d ( 0, -2 )
    -->     , Point2d ( 0, -1 )
    -->     , Point2d ( 1, -1 )
    -->     ]

-}
relativeTo : Frame2d -> Polyline2d -> Polyline2d
relativeTo frame =
    map (Point2d.relativeTo frame)


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Polyline2d.placeIn localFrame stepShape
    --> Polyline2d
    -->     [ Point2d ( 1, 2 )
    -->     , Point2d ( 2, 2 )
    -->     , Point2d ( 2, 3 )
    -->     , Point2d ( 3, 3 )
    -->     ]

-}
placeIn : Frame2d -> Polyline2d -> Polyline2d
placeIn frame =
    map (Point2d.placeIn frame)


{-| Take a polyline defined in 2D coordinates within a particular sketch plane
and return the corresponding polyline in 3D.

    Polyline2d.placeOnto SketchPlane3d.yz stepShape
    --> Polyline3d
    -->     [ Point3d ( 0, 0, 0 )
    -->     , Point3d ( 0, 1, 0 )
    -->     , Point3d ( 0, 1, 1 )
    -->     , Point3d ( 0, 2, 1 )
    -->     ]

-}
placeOnto : SketchPlane3d -> Polyline2d -> Polyline3d
placeOnto sketchPlane =
    vertices >> List.map (Point2d.placeOnto sketchPlane) >> Polyline3d


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline2d.boundingBox stepShape
    --> Just
    -->     (BoundingBox2d
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
