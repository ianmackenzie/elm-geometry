module OpenSolid.Rectangle2d
    exposing
        ( area
        , axes
        , boundingBox
        , centerPoint
        , containing
        , contains
        , dimensions
        , edges
        , in_
        , mirrorAcross
        , placeIn
          --, placeOnto
          --, point
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        , vertices
        , with
        )

import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d


in_ : Frame2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
in_ frame { minX, maxX, minY, maxY } =
    let
        width =
            maxX - minX

        height =
            maxY - minY

        midX =
            minX + 0.5 * width

        midY =
            minY + 0.5 * height

        centerPoint =
            Point2d.in_ frame ( midX, midY )
    in
    Rectangle2d
        { axes = Frame2d.moveTo centerPoint frame
        , dimensions = ( width, height )
        }


containing : Point2d -> Point2d -> Rectangle2d
containing firstPoint secondPoint =
    let
        centerPoint =
            Point2d.midpoint firstPoint secondPoint

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    Rectangle2d
        { axes = Frame2d.at centerPoint
        , dimensions = ( abs (x2 - x1), abs (y2 - y1) )
        }


with : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
with { minX, maxX, minY, maxY } =
    let
        width =
            maxX - minX

        height =
            maxY - minY

        midX =
            minX + 0.5 * width

        midY =
            minY + 0.5 * height

        centerPoint =
            Point2d ( midX, midY )
    in
    Rectangle2d
        { axes = Frame2d.at centerPoint
        , dimensions = ( width, height )
        }


axes : Rectangle2d -> Frame2d
axes (Rectangle2d { axes }) =
    axes


centerPoint : Rectangle2d -> Point2d
centerPoint rectangle =
    Frame2d.originPoint (axes rectangle)


dimensions : Rectangle2d -> ( Float, Float )
dimensions (Rectangle2d { dimensions }) =
    dimensions


area : Rectangle2d -> Float
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width * height


vertices : Rectangle2d -> ( Point2d, Point2d, Point2d, Point2d )
vertices rectangle =
    let
        frame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2
    in
    ( Point2d.in_ frame ( -halfWidth, -halfHeight )
    , Point2d.in_ frame ( halfWidth, -halfHeight )
    , Point2d.in_ frame ( halfWidth, halfHeight )
    , Point2d.in_ frame ( -halfWidth, halfHeight )
    )


contains : Point2d -> Rectangle2d -> Bool
contains point rectangle =
    let
        frame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        ( x, y ) =
            Point2d.coordinates (Point2d.relativeTo frame point)
    in
    abs x <= width / 2 && abs y <= height / 2


edges : Rectangle2d -> ( LineSegment2d, LineSegment2d, LineSegment2d, LineSegment2d )
edges rectangle =
    let
        ( p1, p2, p3, p4 ) =
            vertices rectangle
    in
    ( LineSegment2d ( p1, p2 )
    , LineSegment2d ( p2, p3 )
    , LineSegment2d ( p3, p4 )
    , LineSegment2d ( p4, p1 )
    )


scaleAbout : Point2d -> Float -> Rectangle2d -> Rectangle2d
scaleAbout point scale rectangle =
    let
        currentFrame =
            axes rectangle

        currentXDirection =
            Frame2d.xDirection currentFrame

        currentYDirection =
            Frame2d.yDirection currentFrame

        newCenterPoint =
            Point2d.scaleAbout point scale (Frame2d.originPoint currentFrame)

        newAxes =
            if scale >= 0 then
                Frame2d
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    }
            else
                Frame2d
                    { originPoint = newCenterPoint
                    , xDirection = Direction2d.flip currentXDirection
                    , yDirection = Direction2d.flip currentYDirection
                    }

        ( currentWidth, currentHeight ) =
            dimensions rectangle

        newWidth =
            abs (scale * currentWidth)

        newHeight =
            abs (scale * currentHeight)
    in
    Rectangle2d
        { axes = newAxes
        , dimensions = ( newWidth, newHeight )
        }


rotateAround : Point2d -> Float -> Rectangle2d -> Rectangle2d
rotateAround point angle =
    let
        rotateFrame =
            Frame2d.rotateAround point angle
    in
    \rectangle ->
        Rectangle2d
            { axes = rotateFrame (axes rectangle)
            , dimensions = dimensions rectangle
            }


translateBy : Vector2d -> Rectangle2d -> Rectangle2d
translateBy displacement rectangle =
    Rectangle2d
        { axes = Frame2d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


mirrorAcross : Axis2d -> Rectangle2d -> Rectangle2d
mirrorAcross axis rectangle =
    Rectangle2d
        { axes = Frame2d.mirrorAcross axis (axes rectangle)
        , dimensions = dimensions rectangle
        }


placeIn : Frame2d -> Rectangle2d -> Rectangle2d
placeIn frame rectangle =
    Rectangle2d
        { axes = Frame2d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


relativeTo : Frame2d -> Rectangle2d -> Rectangle2d
relativeTo frame rectangle =
    Rectangle2d
        { axes = Frame2d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


boundingBox : Rectangle2d -> BoundingBox2d
boundingBox rectangle =
    let
        ( p1, p2, p3, p4 ) =
            vertices rectangle
    in
    BoundingBox2d.hull (Point2d.hull p1 p2) (Point2d.hull p3 p4)
