{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Polyline2d
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
        , placeOnto
        , boundingBox
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d


vertices : Polyline2d -> List Point2d
vertices (Polyline2d vertices') =
    vertices'


segments : Polyline2d -> List LineSegment2d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment2d ( start, end )) all rest


length : Polyline2d -> Float
length =
    segments >> List.map LineSegment2d.length >> List.sum


map : (Point2d -> Point2d) -> Polyline2d -> Polyline2d
map function =
    vertices >> List.map function >> Polyline2d


scaleAbout : Point2d -> Float -> Polyline2d -> Polyline2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> Polyline2d -> Polyline2d
rotateAround point angle =
    map (Point2d.rotateAround point angle)


translateBy : Vector2d -> Polyline2d -> Polyline2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> Polyline2d -> Polyline2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


relativeTo : Frame2d -> Polyline2d -> Polyline2d
relativeTo frame =
    map (Point2d.relativeTo frame)


placeIn : Frame2d -> Polyline2d -> Polyline2d
placeIn frame =
    map (Point2d.placeIn frame)


placeOnto : SketchPlane3d -> Polyline2d -> Polyline3d
placeOnto sketchPlane =
    vertices >> List.map (Point2d.placeOnto sketchPlane) >> Polyline3d


boundingBox : Polyline2d -> Maybe BoundingBox2d
boundingBox polyline =
    BoundingBox2d.containing (vertices polyline)
