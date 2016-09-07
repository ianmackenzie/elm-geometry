module OpenSolid.BoundingBox2d
    exposing
        ( singleton
        , containing2
        , containing3
        , containing
        , minX
        , maxX
        , minY
        , maxY
        , minPoint
        , maxPoint
        , midpoint
        , overlaps
        , contains
        , hull
        , intersection
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d


singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        BoundingBox2d { minX = x, maxX = x, minY = y, maxY = y }


containing2 : ( Point2d, Point2d ) -> BoundingBox2d
containing2 points =
    let
        ( firstPoint, secondPoint ) =
            points

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


containing3 : ( Point2d, Point2d, Point2d ) -> BoundingBox2d
containing3 points =
    let
        ( firstPoint, secondPoint, thirdPoint ) =
            points

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint

        ( x3, y3 ) =
            Point2d.coordinates thirdPoint
    in
        BoundingBox2d
            { minX = min x1 (min x2 x3)
            , maxX = max x1 (max x2 x3)
            , minY = min y1 (min y2 y3)
            , maxY = max y1 (max y2 y3)
            }


containing : List Point2d -> Maybe BoundingBox2d
containing points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl hull (singleton first) (List.map singleton rest))


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


midpoint : BoundingBox2d -> Point2d
midpoint boundingBox =
    Point2d.midpoint (minPoint boundingBox) (maxPoint boundingBox)


overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


contains : BoundingBox2d -> BoundingBox2d -> Bool
contains other boundingBox =
    (minX boundingBox <= minX other)
        && (maxX boundingBox >= maxX other)
        && (minY boundingBox <= minY other)
        && (maxY boundingBox >= maxY other)


hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull other boundingBox =
    BoundingBox2d
        { minX = min (minX boundingBox) (minX other)
        , maxX = max (maxX boundingBox) (maxX other)
        , minY = min (minY boundingBox) (minY other)
        , maxY = max (maxY boundingBox) (maxY other)
        }


intersection : BoundingBox2d -> BoundingBox2d -> Maybe BoundingBox2d
intersection other boundingBox =
    if overlaps other boundingBox then
        Just
            (BoundingBox2d
                { minX = max (minX boundingBox) (minX other)
                , maxX = min (maxX boundingBox) (maxX other)
                , minY = max (minY boundingBox) (minY other)
                , maxY = min (maxY boundingBox) (maxY other)
                }
            )
    else
        Nothing
