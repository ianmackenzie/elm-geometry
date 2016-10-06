{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.LineSegment2d
    exposing
        ( endpoints
        , startPoint
        , endPoint
        , midpoint
        , interpolate
        , map
        , mapTo
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
        , boundingBox
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d


endpoints : LineSegment2d -> ( Point2d, Point2d )
endpoints (LineSegment2d endpoints') =
    endpoints'


startPoint : LineSegment2d -> Point2d
startPoint (LineSegment2d ( start, _ )) =
    start


endPoint : LineSegment2d -> Point2d
endPoint (LineSegment2d ( _, end )) =
    end


midpoint : LineSegment2d -> Point2d
midpoint lineSegment =
    interpolate lineSegment 0.5


interpolate : LineSegment2d -> Float -> Point2d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
        Point2d.interpolate start end


map : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
map =
    mapTo LineSegment2d


mapTo : (( a, a ) -> b) -> (Point2d -> a) -> LineSegment2d -> b
mapTo tag map lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        tag ( map p1, map p2 )


vector : LineSegment2d -> Vector2d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point2d.vectorFrom p1 p2


direction : LineSegment2d -> Maybe Direction2d
direction =
    vector >> Vector2d.direction


normalDirection : LineSegment2d -> Maybe Direction2d
normalDirection =
    vector >> Vector2d.perpendicularTo >> Vector2d.direction


squaredLength : LineSegment2d -> Float
squaredLength =
    vector >> Vector2d.squaredLength


length : LineSegment2d -> Float
length =
    vector >> Vector2d.length


scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
    map (Point2d.projectOnto axis)


relativeTo : Frame2d -> LineSegment2d -> LineSegment2d
relativeTo frame =
    map (Point2d.relativeTo frame)


placeIn : Frame2d -> LineSegment2d -> LineSegment2d
placeIn frame =
    map (Point2d.placeIn frame)


boundingBox : LineSegment2d -> BoundingBox2d
boundingBox lineSegment =
    BoundingBox2d.containing2 (endpoints lineSegment)
