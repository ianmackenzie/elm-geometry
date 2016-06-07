{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Point2d
    exposing
        ( origin
        , along
        , polar
        , fromCoordinates
        , fromPolarCoordinates
        , interpolate
        , midpoint
        , xCoordinate
        , yCoordinate
        , coordinates
        , polarCoordinates
        , squaredDistanceFrom
        , distanceFrom
        , vectorTo
        , vectorFrom
        , distanceAlong
        , distanceFromAxis
        , scaleAbout
        , rotateAround
        , translateAlong
        , mirrorAcross
        , toLocalIn
        , fromLocalIn
        , projectOnto
        , plus
        , minus
        , toRecord
        , fromRecord
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d


origin : Point2d
origin =
    Point2d 0 0


along : Axis2d -> Float -> Point2d
along axis distance =
    plus (Vector2d.along axis distance) axis.originPoint


polar : Float -> Float -> Point2d
polar radius angle =
    fromPolarCoordinates ( radius, angle )


fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates ( x, y ) =
    Point2d x y


fromPolarCoordinates : ( Float, Float ) -> Point2d
fromPolarCoordinates =
    fromPolar >> fromCoordinates


interpolate : Point2d -> Point2d -> Float -> Point2d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> plus (Vector2d.times t displacement) startPoint


midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


xCoordinate : Point2d -> Float
xCoordinate (Point2d x _) =
    x


yCoordinate : Point2d -> Float
yCoordinate (Point2d _ y) =
    y


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d x y) =
    ( x, y )


polarCoordinates : Point2d -> ( Float, Float )
polarCoordinates =
    coordinates >> toPolar


squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector2d.squaredLength


distanceFrom : Point2d -> Point2d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


vectorTo : Point2d -> Point2d -> Vector2d
vectorTo (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x2 - x1) (y2 - y1)


vectorFrom : Point2d -> Point2d -> Vector2d
vectorFrom (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


distanceAlong : Axis2d -> Point2d -> Float
distanceAlong axis =
    vectorFrom axis.originPoint >> Vector2d.componentIn axis.direction


distanceFromAxis : Axis2d -> Point2d -> Float
distanceFromAxis axis =
    vectorFrom axis.originPoint
        >> Vector2d.crossProduct (Direction2d.asVector axis.direction)
        >> abs


addTo (Point2d px py) (Vector2d vx vy) =
    Point2d (px + vx) (py + vy)


scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector2d.times scale >> addTo centerPoint


rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    vectorFrom centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


translateAlong : Axis2d -> Float -> Point2d -> Point2d
translateAlong axis distance =
    plus (Vector2d.along axis distance)


mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    vectorFrom axis.originPoint
        >> Vector2d.mirrorAcross axis
        >> addTo axis.originPoint


toLocalIn : Frame2d -> Point2d -> Point2d
toLocalIn frame =
    vectorFrom frame.originPoint
        >> Vector2d.toLocalIn frame
        >> (\(Vector2d x y) -> Point2d x y)


fromLocalIn : Frame2d -> Point2d -> Point2d
fromLocalIn frame =
    (\(Point2d x y) -> Vector2d x y)
        >> Vector2d.fromLocalIn frame
        >> addTo frame.originPoint


projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    vectorFrom axis.originPoint
        >> Vector2d.projectOnto axis
        >> addTo axis.originPoint


plus : Vector2d -> Point2d -> Point2d
plus (Vector2d vx vy) (Point2d px py) =
    Point2d (px + vx) (py + vy)


minus : Vector2d -> Point2d -> Point2d
minus (Vector2d vx vy) (Point2d px py) =
    Point2d (px - vx) (py - vy)


toRecord : Point2d -> { x : Float, y : Float }
toRecord (Point2d x y) =
    { x = x, y = y }


fromRecord : { x : Float, y : Float } -> Point2d
fromRecord { x, y } =
    Point2d x y
