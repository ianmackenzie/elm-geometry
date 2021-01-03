module Geometry.Random exposing
    ( angle
    , length
    , parameterValue
    , positiveLength
    , scale
    , vector2d
    , vector3d
    , vector4d
    , vectorBoundingBox2d
    , vectorBoundingBox3d
    , vectorBoundingBox4d
    )

import Angle exposing (Angle)
import Length exposing (Length, Meters)
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Vector4d exposing (Vector4d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)
import VectorBoundingBox4d exposing (VectorBoundingBox4d)


parameterValue : Generator Float
parameterValue =
    Random.weighted
        ( 1, Random.float 0 1 )
        [ ( 1, Random.constant 0 )
        , ( 1, Random.constant 1 )
        ]
        |> Random.andThen identity


scale : Generator Float
scale =
    Random.weighted
        ( 3, Random.float -10 10 )
        [ ( 1, Random.constant 0 ) ]
        |> Random.andThen identity


length : Generator Length
length =
    Random.map Length.meters (Random.float -10 10)


positiveLength : Generator Length
positiveLength =
    Random.map Quantity.abs length


angle : Generator Angle
angle =
    Random.map Angle.radians (Random.float (-2 * pi) (2 * pi))


vector2d : Generator (Vector2d Meters coordinates)
vector2d =
    Random.map2 Vector2d.xy length length


vector3d : Generator (Vector3d Meters coordinates)
vector3d =
    Random.map3 Vector3d.xyz length length length


vector4d : Generator (Vector4d Meters coordinates)
vector4d =
    Random.map4 Vector4d.xyzw length length length length


vectorBoundingBox2d : Generator (VectorBoundingBox2d Meters coordinates)
vectorBoundingBox2d =
    Random.map2 VectorBoundingBox2d.from vector2d vector2d


vectorBoundingBox3d : Generator (VectorBoundingBox3d Meters coordinates)
vectorBoundingBox3d =
    Random.map2 VectorBoundingBox3d.from vector3d vector3d


vectorBoundingBox4d : Generator (VectorBoundingBox4d Meters coordinates)
vectorBoundingBox4d =
    Random.map2 VectorBoundingBox4d.from vector4d vector4d
