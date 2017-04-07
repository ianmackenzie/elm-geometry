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


module OpenSolid.Polyline3d
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
        , projectInto
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/polyline3d.svg" alt="Polyline3d" width="160">

A `Polyline3d` represents a sequence of vertices connected by line segments.
This module contains a variety of polyline-related functionality, such as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

Polylines can be constructed by passing an ordered list of vertices to the
`Polyline3d` constructor, for example

    examplePolyline =
        Polyline3d
            [ Point2d ( 0, 0, 0 )
            , Point2d ( 1, 0, 0 )
            , Point2d ( 1, 2, 0 )
            , Point2d ( 1, 2, 3 )
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

@docs projectInto


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.LineSegment3d as LineSegment3d


{-| Get the vertices of a polyline.

    Polyline3d.vertices examplePolyline
    --> [ Point3d ( 0, 0, 0 )
    --> , Point3d ( 1, 0, 0 )
    --> , Point3d ( 1, 2, 0 )
    --> , Point3d ( 1, 2, 3 )
    --> ]

-}
vertices : Polyline3d -> List Point3d
vertices (Polyline3d vertices_) =
    vertices_


{-| Get the individual segments of a polyline.

    Polyline3d.segments examplePolyline
    --> [ LineSegment3d ( Point3d ( 0, 0, 0 ), Point3d ( 1, 0, 0 ) )
    --> , LineSegment3d ( Point3d ( 1, 0, 0 ), Point3d ( 1, 2, 0 ) )
    --> , LineSegment3d ( Point3d ( 1, 2, 0 ), Point3d ( 1, 2, 3 ) )
    --> ]

-}
segments : Polyline3d -> List LineSegment3d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment3d ( start, end )) all rest


{-| Get the overall length of a polyline (the sum of the lengths of its
segments).

    Polyline3d.length examplePolyline
    --> 6

-}
length : Polyline3d -> Float
length =
    segments >> List.map LineSegment3d.length >> List.sum


{-| Scale a polyline about the given center point by the given scale.

    point =
        Point3d ( 1, 0, 0 )

    Polyline3d.scaleAbout point 2 examplePolyline
    --> Polyline3d
    -->     [ Point3d ( -1, 0, 0 )
    -->     , Point3d ( 1, 0, 0 )
    -->     , Point3d ( 1, 4, 0 )
    -->     , Point3d ( 1, 4, 6 )
    -->     ]

-}
scaleAbout : Point3d -> Float -> Polyline3d -> Polyline3d
scaleAbout point scale =
    map (Point3d.scaleAbout point scale)


{-| Rotate a polyline around the given axis by the given angle (in radians).

    Polyline3d.rotateAround Axis3d.z (degrees 90) examplePolyline
    --> Polyline3d
    -->     [ Point3d ( 0, 0, 0 )
    -->     , Point3d ( 0, 1, 0 )
    -->     , Point3d ( -2, 1, 0 )
    -->     , Point3d ( -2, 1, 3 )
    -->     ]

-}
rotateAround : Axis3d -> Float -> Polyline3d -> Polyline3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


{-| Translate a polyline by the given displacement.

    displacement =
        Vector3d ( 1, 2, 3 )

    Polyline3d.translateBy displacement examplePolyline
    --> Polyline3d
    -->     [ Point3d ( 1, 2, 3 )
    -->     , Point3d ( 2, 2, 3 )
    -->     , Point3d ( 2, 4, 3 )
    -->     , Point3d ( 2, 4, 6 )
    -->     ]

-}
translateBy : Vector3d -> Polyline3d -> Polyline3d
translateBy vector =
    map (Point3d.translateBy vector)


{-| Mirror a polyline across the given plane.

    Polyline3d.mirrorAcross Plane3d.xz examplePolyline
    --> Polyline3d
    -->     [ Point3d ( 0, 0, 0 )
    -->     , Point3d ( 1, 0, 0 )
    -->     , Point3d ( 1, -2, 0 )
    -->     , Point3d ( 1, -2, 3 )
    -->     ]

-}
mirrorAcross : Plane3d -> Polyline3d -> Polyline3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


{-| Project (flatten) a polyline onto the given plane.

    Polyline3d.projectOnto Plane3d.xz examplePolyline
    --> Polyline3d
    -->     [ Point3d ( 0, 0, 0 )
    -->     , Point3d ( 1, 0, 0 )
    -->     , Point3d ( 1, 0, 0 )
    -->     , Point3d ( 1, 0, 3 )
    -->     ]

-}
projectOnto : Plane3d -> Polyline3d -> Polyline3d
projectOnto plane =
    map (Point3d.projectOnto plane)


{-| Transform each vertex of a polyline by the given function. All other
transformations can be defined in terms of `map`; for example,

    Polyline3d.mirrorAcross Plane3d.xz polyline

is equivalent to

    Polyline3d.map (Point3d.mirrorAcross Plane3d.xz) polyline

-}
map : (Point3d -> Point3d) -> Polyline3d -> Polyline3d
map function =
    vertices >> List.map function >> Polyline3d


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Polyline3d.relativeTo localFrame examplePolyline
    --> Polyline3d
    -->     [ Point3d ( -1, -2, -3 )
    -->     , Point3d ( 0, -2, -3 )
    -->     , Point3d ( 0, 0, -3 )
    -->     , Point3d ( 0, 0, 0 )
    -->     ]

-}
relativeTo : Frame3d -> Polyline3d -> Polyline3d
relativeTo frame =
    map (Point3d.relativeTo frame)


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    Polyline3d.placeIn localFrame examplePolyline
    --> Polyline3d
    -->     [ Point3d ( 1, 2, 3 )
    -->     , Point3d ( 2, 2, 3 )
    -->     , Point3d ( 2, 4, 3 )
    -->     , Point3d ( 2, 4, 6 )
    -->     ]

-}
placeIn : Frame3d -> Polyline3d -> Polyline3d
placeIn frame =
    map (Point3d.placeIn frame)


{-| Project a polyline into a given sketch plane. Conceptually, this projects
the polyline onto the plane and then expresses the projected polyline in 2D
sketch coordinates.

    Polyline3d.projectInto Plane3d.xy examplePolyline
    --> Polyline2d
    -->     [ Point2d ( 0, 0 )
    -->     , Point2d ( 1, 0 )
    -->     , Point2d ( 1, 2 )
    -->     , Point2d ( 1, 2 )
    -->     ]

-}
projectInto : SketchPlane3d -> Polyline3d -> Polyline2d
projectInto sketchPlane =
    vertices >> List.map (Point3d.projectInto sketchPlane) >> Polyline2d


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline3d.boundingBox examplePolyline
    --> Just
    -->     (BoundingBox3d
    -->         { minX = 0
    -->         , maxX = 1
    -->         , minY = 0
    -->         , maxY = 2
    -->         , minZ = 0
    -->         , maxZ = 3
    -->         }
    -->     )

-}
boundingBox : Polyline3d -> Maybe BoundingBox3d
boundingBox polyline =
    BoundingBox3d.containing (vertices polyline)
