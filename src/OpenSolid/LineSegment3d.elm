{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.LineSegment3d
    exposing
        ( endpoints
        , startPoint
        , endPoint
        , reverse
        , midpoint
        , interpolate
        , map
        , vector
        , direction
        , normalDirection
        , squaredLength
        , length
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , relativeTo
        , placeIn
        , projectInto
        , placeOnto
        , boundingBox
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Point3d as Point3d
import OpenSolid.LineSegment2d as LineSegment2d


endpoints : LineSegment3d -> ( Point3d, Point3d )
endpoints (LineSegment3d endpoints') =
    endpoints'


startPoint : LineSegment3d -> Point3d
startPoint (LineSegment3d ( start, _ )) =
    start


endPoint : LineSegment3d -> Point3d
endPoint (LineSegment3d ( _, end )) =
    end


reverse : LineSegment3d -> LineSegment3d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( p2, p1 )


midpoint : LineSegment3d -> Point3d
midpoint lineSegment =
    interpolate lineSegment 0.5


interpolate : LineSegment3d -> Float -> Point3d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
        Point3d.interpolate start end


map : (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
map function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( function p1, function p2 )


vector : LineSegment3d -> Vector3d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.vectorFrom p1 p2


direction : LineSegment3d -> Maybe Direction3d
direction =
    vector >> Vector3d.direction


normalDirection : LineSegment3d -> Maybe Direction3d
normalDirection =
    vector >> Vector3d.perpendicularTo >> Vector3d.direction


squaredLength : LineSegment3d -> Float
squaredLength =
    vector >> Vector3d.squaredLength


length : LineSegment3d -> Float
length =
    vector >> Vector3d.length


scaleAbout : Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
    map (Point3d.scaleAbout point scale)


rotateAround : Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


translateBy : Vector3d -> LineSegment3d -> LineSegment3d
translateBy vector =
    map (Point3d.translateBy vector)


mirrorAcross : Plane3d -> LineSegment3d -> LineSegment3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


projectOnto : Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
    map (Point3d.projectOnto plane)


relativeTo : Frame3d -> LineSegment3d -> LineSegment3d
relativeTo frame =
    map (Point3d.relativeTo frame)


placeIn : Frame3d -> LineSegment3d -> LineSegment3d
placeIn frame =
    map (Point3d.placeIn frame)


projectInto : SketchPlane3d -> LineSegment3d -> LineSegment2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
        LineSegment2d ( project p1, project p2 )


placeOnto : SketchPlane3d -> LineSegment2d -> LineSegment3d
placeOnto sketchPlane lineSegment2d =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment2d

        place =
            Point3d.placeOnto sketchPlane
    in
        LineSegment3d ( place p1, place p2 )


boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.hull p1 p2
