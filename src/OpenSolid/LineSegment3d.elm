module OpenSolid.LineSegment3d
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
        , projectOntoAxis
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
import OpenSolid.BoundingBox3d as BoundingBox3d
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
map =
    mapTo LineSegment3d


mapTo : (( a, a ) -> b) -> (Point3d -> a) -> LineSegment3d -> b
mapTo tag map lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        tag ( map p1, map p2 )


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


projectOntoAxis : Axis3d -> LineSegment3d -> LineSegment3d
projectOntoAxis axis =
    map (Point3d.projectOntoAxis axis)


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
projectInto sketchPlane =
    mapTo LineSegment2d (Point3d.projectInto sketchPlane)


placeOnto : SketchPlane3d -> LineSegment2d -> LineSegment3d
placeOnto sketchPlane =
    LineSegment2d.mapTo LineSegment3d (Point3d.placeOnto sketchPlane)


boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    BoundingBox3d.containing2 (endpoints lineSegment)
