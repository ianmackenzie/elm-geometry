module OpenSolid.Rectangle2d
    exposing
        ( Rectangle2d
        , area
        , axes
        , boundingBox
        , centerPoint
        , centeredOn
        , containing
        , contains
        , dimensions
        , edges
        , fromExtrema
        , fromExtremaIn
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        , vertices
        )

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Internal as Internal
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


type alias Rectangle2d =
    Internal.Rectangle2d


centeredOn : Frame2d -> ( Float, Float ) -> Rectangle2d
centeredOn axes dimensions =
    Internal.Rectangle2d { axes = axes, dimensions = dimensions }


fromExtremaIn : Frame2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtremaIn localFrame { minX, maxX, minY, maxY } =
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
            Point2d.fromCoordinatesIn localFrame ( midX, midY )
    in
    Internal.Rectangle2d
        { axes = Frame2d.moveTo centerPoint localFrame
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
    Internal.Rectangle2d
        { axes = Frame2d.atPoint centerPoint
        , dimensions = ( abs (x2 - x1), abs (y2 - y1) )
        }


fromExtrema : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtrema { minX, maxX, minY, maxY } =
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
            Point2d.fromCoordinates ( midX, midY )
    in
    Internal.Rectangle2d
        { axes = Frame2d.atPoint centerPoint
        , dimensions = ( width, height )
        }


axes : Rectangle2d -> Frame2d
axes (Internal.Rectangle2d { axes }) =
    axes


centerPoint : Rectangle2d -> Point2d
centerPoint rectangle =
    Frame2d.originPoint (axes rectangle)


dimensions : Rectangle2d -> ( Float, Float )
dimensions (Internal.Rectangle2d { dimensions }) =
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
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2
    in
    ( Point2d.fromCoordinatesIn localFrame ( -halfWidth, -halfHeight )
    , Point2d.fromCoordinatesIn localFrame ( halfWidth, -halfHeight )
    , Point2d.fromCoordinatesIn localFrame ( halfWidth, halfHeight )
    , Point2d.fromCoordinatesIn localFrame ( -halfWidth, halfHeight )
    )


contains : Point2d -> Rectangle2d -> Bool
contains point rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        ( x, y ) =
            Point2d.coordinates (Point2d.relativeTo localFrame point)
    in
    abs x <= width / 2 && abs y <= height / 2


edges : Rectangle2d -> ( LineSegment2d, LineSegment2d, LineSegment2d, LineSegment2d )
edges rectangle =
    let
        ( p1, p2, p3, p4 ) =
            vertices rectangle
    in
    ( LineSegment2d.from p1 p2
    , LineSegment2d.from p2 p3
    , LineSegment2d.from p3 p4
    , LineSegment2d.from p4 p1
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
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    }
            else
                Frame2d.unsafe
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
    Internal.Rectangle2d
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
        Internal.Rectangle2d
            { axes = rotateFrame (axes rectangle)
            , dimensions = dimensions rectangle
            }


translateBy : Vector2d -> Rectangle2d -> Rectangle2d
translateBy displacement rectangle =
    Internal.Rectangle2d
        { axes = Frame2d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


mirrorAcross : Axis2d -> Rectangle2d -> Rectangle2d
mirrorAcross axis rectangle =
    Internal.Rectangle2d
        { axes = Frame2d.mirrorAcross axis (axes rectangle)
        , dimensions = dimensions rectangle
        }


placeIn : Frame2d -> Rectangle2d -> Rectangle2d
placeIn frame rectangle =
    Internal.Rectangle2d
        { axes = Frame2d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


relativeTo : Frame2d -> Rectangle2d -> Rectangle2d
relativeTo frame rectangle =
    Internal.Rectangle2d
        { axes = Frame2d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


boundingBox : Rectangle2d -> BoundingBox2d
boundingBox rectangle =
    let
        ( p1, p2, p3, p4 ) =
            vertices rectangle

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        ( x4, y4 ) =
            Point2d.coordinates p4
    in
    BoundingBox2d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        }
