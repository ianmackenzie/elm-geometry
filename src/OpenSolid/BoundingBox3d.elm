module OpenSolid.BoundingBox3d
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
        , minZ
        , maxZ
        , minPoint
        , maxPoint
        , isEmpty
        , isWhole
        , midpoint
        , overlaps
        , isContainedWithin
        , hull
        , intersection
        , encode
        , decoder
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Encode as Encode
import OpenSolid.Decode as Decode


empty : BoundingBox3d
empty =
    BoundingBox3d
        { minX = 0 / 0
        , maxX = 0 / 0
        , minY = 0 / 0
        , maxY = 0 / 0
        , minZ = 0 / 0
        , maxZ = 0 / 0
        }


whole : BoundingBox3d
whole =
    BoundingBox3d
        { minX = -1 / 0
        , maxX = 1 / 0
        , minY = -1 / 0
        , maxY = 1 / 0
        , minZ = -1 / 0
        , maxZ = 1 / 0
        }


singleton : Point3d -> BoundingBox3d
singleton point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
        BoundingBox3d
            { minX = x
            , maxX = x
            , minY = y
            , maxY = y
            , minZ = z
            , maxZ = z
            }


fromPoints : Point3d -> Point3d -> BoundingBox3d
fromPoints firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
        BoundingBox3d
            { minX = min x1 x2
            , maxX = max x1 x2
            , minY = min y1 y2
            , maxY = max y1 y2
            , minZ = min z1 z2
            , maxZ = max z1 z2
            }


containing : List Point3d -> BoundingBox3d
containing points =
    List.foldl hull empty (List.map singleton points)


minX : BoundingBox3d -> Float
minX (BoundingBox3d properties) =
    properties.minX


maxX : BoundingBox3d -> Float
maxX (BoundingBox3d properties) =
    properties.maxX


minY : BoundingBox3d -> Float
minY (BoundingBox3d properties) =
    properties.minY


maxY : BoundingBox3d -> Float
maxY (BoundingBox3d properties) =
    properties.maxY


minZ : BoundingBox3d -> Float
minZ (BoundingBox3d properties) =
    properties.minZ


maxZ : BoundingBox3d -> Float
maxZ (BoundingBox3d properties) =
    properties.maxZ


minPoint : BoundingBox3d -> Point3d
minPoint boundingBox =
    Point3d ( minX boundingBox, minY boundingBox, minZ boundingBox )


maxPoint : BoundingBox3d -> Point3d
maxPoint boundingBox =
    Point3d ( maxX boundingBox, maxY boundingBox, maxZ boundingBox )


isEmpty : BoundingBox3d -> Bool
isEmpty (BoundingBox3d { minX, maxX, minY, maxY, minZ, maxZ }) =
    not ((minX <= maxX) && (minY <= maxY) && (minZ <= maxZ))


isWhole : BoundingBox3d -> Bool
isWhole boundingBox =
    boundingBox == whole


midpoint : BoundingBox3d -> Point3d
midpoint boundingBox =
    Point3d.midpoint (minPoint boundingBox) (maxPoint boundingBox)


overlaps : BoundingBox3d -> BoundingBox3d -> Bool
overlaps other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)
        && (minZ boundingBox <= maxZ other)
        && (maxZ boundingBox >= minZ other)


isContainedWithin : BoundingBox3d -> BoundingBox3d -> Bool
isContainedWithin other boundingBox =
    (minX boundingBox >= minX other)
        && (maxX boundingBox <= maxX other)
        && (minY boundingBox >= minY other)
        && (maxY boundingBox <= maxY other)
        && (minZ boundingBox >= minZ other)
        && (maxZ boundingBox <= maxZ other)


hull : BoundingBox3d -> BoundingBox3d -> BoundingBox3d
hull other boundingBox =
    if isEmpty other then
        boundingBox
    else if isEmpty boundingBox then
        other
    else
        BoundingBox3d
            { minX = min (minX boundingBox) (minX other)
            , maxX = max (maxX boundingBox) (maxX other)
            , minY = min (minY boundingBox) (minY other)
            , maxY = max (maxY boundingBox) (maxY other)
            , minZ = min (minZ boundingBox) (minZ other)
            , maxZ = max (maxZ boundingBox) (maxZ other)
            }


intersection : BoundingBox3d -> BoundingBox3d -> BoundingBox3d
intersection other boundingBox =
    if overlaps other boundingBox then
        BoundingBox3d
            { minX = max (minX boundingBox) (minX other)
            , maxX = min (maxX boundingBox) (maxX other)
            , minY = max (minY boundingBox) (minY other)
            , maxY = min (maxY boundingBox) (maxY other)
            , minZ = max (minZ boundingBox) (minZ other)
            , maxZ = min (maxZ boundingBox) (maxZ other)
            }
    else
        empty


encode : BoundingBox3d -> Value
encode boundingBox =
    Encode.object
        [ ( "minX", Encode.extremum (minX boundingBox) )
        , ( "maxX", Encode.extremum (maxX boundingBox) )
        , ( "minY", Encode.extremum (minY boundingBox) )
        , ( "maxY", Encode.extremum (maxY boundingBox) )
        , ( "minZ", Encode.extremum (minZ boundingBox) )
        , ( "maxZ", Encode.extremum (maxZ boundingBox) )
        ]


decoder : Decoder BoundingBox3d
decoder =
    Decode.object6
        (\minX maxX minY maxY minZ maxZ ->
            BoundingBox3d
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                , minZ = minZ
                , maxZ = maxZ
                }
        )
        ("minX" := Decode.extremum)
        ("maxX" := Decode.extremum)
        ("minY" := Decode.extremum)
        ("maxY" := Decode.extremum)
        ("minZ" := Decode.extremum)
        ("maxZ" := Decode.extremum)
