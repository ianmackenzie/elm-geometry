{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Polyline3d
    exposing
        ( vertices
        , segments
        , length
        , map
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , projectInto
        , boundingBox
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.LineSegment3d as LineSegment3d


vertices : Polyline3d -> List Point3d
vertices (Polyline3d vertices') =
    vertices'


segments : Polyline3d -> List LineSegment3d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment3d ( start, end )) all rest


length : Polyline3d -> Float
length =
    segments >> List.map LineSegment3d.length >> List.sum


map : (Point3d -> Point3d) -> Polyline3d -> Polyline3d
map function =
    vertices >> List.map function >> Polyline3d


scaleAbout : Point3d -> Float -> Polyline3d -> Polyline3d
scaleAbout point scale =
    map (Point3d.scaleAbout point scale)


rotateAround : Axis3d -> Float -> Polyline3d -> Polyline3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


translateBy : Vector3d -> Polyline3d -> Polyline3d
translateBy vector =
    map (Point3d.translateBy vector)


mirrorAcross : Plane3d -> Polyline3d -> Polyline3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


relativeTo : Frame3d -> Polyline3d -> Polyline3d
relativeTo frame =
    map (Point3d.relativeTo frame)


placeIn : Frame3d -> Polyline3d -> Polyline3d
placeIn frame =
    map (Point3d.placeIn frame)


projectInto : SketchPlane3d -> Polyline3d -> Polyline2d
projectInto sketchPlane =
    vertices >> List.map (Point3d.projectInto sketchPlane) >> Polyline2d


boundingBox : Polyline3d -> Maybe BoundingBox3d
boundingBox polyline =
    BoundingBox3d.containing (vertices polyline)
