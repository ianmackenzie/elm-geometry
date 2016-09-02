module OpenSolid.BoundingBox.BoundingBox2d
    exposing
        ( empty
        , whole
        , singleton
        , fromPoints
        , containing
        , minX
        , maxX
        , minY
        , maxY
        , minPoint
        , maxPoint
        , isEmpty
        , isWhole
        , midpoint
        , contains
        , overlaps
        , isContainedWithin
        , hull
        , intersection
        )

import OpenSolid.Types exposing (..)
import OpenSolid.Point2d as Point2d


empty : BoundingBox2d
empty =
    BoundingBox2d
        { minX = 0 / 0
        , maxX = 0 / 0
        , minY = 0 / 0
        , maxY = 0 / 0
        }


whole : BoundingBox2d
whole =
    BoundingBox2d
        { minX = -1 / 0
        , maxX = 1 / 0
        , minY = -1 / 0
        , maxY = 1 / 0
        }


singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        BoundingBox2d { minX = x, maxX = x, minY = y, maxY = y }


fromPoints : Point2d -> Point2d -> BoundingBox2d
fromPoints firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
        BoundingBox2d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            }


containing : List Point2d -> BoundingBox2d
containing points =
    List.foldl hull empty (List.map singleton points)


minX : BoundingBox2d -> Float
minX (BoundingBox2d properties) =
    properties.minX


maxX : BoundingBox2d -> Float
maxX (BoundingBox2d properties) =
    properties.maxX


minY : BoundingBox2d -> Float
minY (BoundingBox2d properties) =
    properties.minY


maxY : BoundingBox2d -> Float
maxY (BoundingBox2d properties) =
    properties.maxY


minPoint : BoundingBox2d -> Point2d
minPoint boundingBox =
    Point2d ( minX boundingBox, minY boundingBox )


maxPoint : BoundingBox2d -> Point2d
maxPoint boundingBox =
    Point2d ( maxX boundingBox, maxY boundingBox )


isEmpty : BoundingBox2d -> Bool
isEmpty (BoundingBox2d { minX, maxX, minY, maxY }) =
    not ((minX <= maxX) && (minY <= maxY))


isWhole : BoundingBox2d -> Bool
isWhole boundingBox =
    boundingBox == whole


midpoint : BoundingBox2d -> Point2d
midpoint boundingBox =
    Point2d.midpoint (minPoint boundingBox) (maxPoint boundingBox)


overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


isContainedWithin : BoundingBox2d -> BoundingBox2d -> Bool
isContainedWithin other boundingBox =
    (minX boundingBox >= minX other)
        && (maxX boundingBox <= maxX other)
        && (minY boundingBox >= minY other)
        && (maxY boundingBox <= maxY other)


hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull other boundingBox =
    if isEmpty other then
        boundingBox
    else if isEmpty boundingBox then
        other
    else
        BoundingBox2d
            { minX = min (minX boundingBox) (minX other)
            , maxX = max (maxX boundingBox) (maxX other)
            , minY = min (minY boundingBox) (minY other)
            , maxY = max (maxY boundingBox) (maxY other)
            }


intersection : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
intersection other boundingBox =
    if overlaps other boundingBox then
        BoundingBox2d
            { minX = max (minX boundingBox) (minX other)
            , maxX = min (maxX boundingBox) (maxX other)
            , minY = max (minY boundingBox) (minY other)
            , maxY = min (maxY boundingBox) (maxY other)
            }
    else
        empty
