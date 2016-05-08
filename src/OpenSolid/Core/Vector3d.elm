module OpenSolid.Core.Vector3d (zero, fromComponents, xComponent, yComponent, zComponent, components, equals, componentIn, squaredLength, length, normalize, direction, perpendicularVector, normalDirection, rotateAbout, mirrorAlong, relativeTo, placeIn, projectionIn, projectOnto, projectInto, negate, plus, minus, times, addTo, subtractFrom, dot, cross) where

import OpenSolid.Core exposing (..)


zero : Vector3d
zero =
  Vector3d 0 0 0


fromComponents : ( Float, Float, Float ) -> Vector3d
fromComponents ( x, y, z ) =
  Vector3d x y z


xComponent : Vector3d -> Float
xComponent (Vector3d x _ _) =
  x


yComponent : Vector3d -> Float
yComponent (Vector3d _ y _) =
  y


zComponent : Vector3d -> Float
zComponent (Vector3d _ _ z) =
  z


components : Vector3d -> ( Float, Float, Float )
components (Vector3d x y z) =
  ( x, y, z )


equals : Vector3d -> Vector3d -> Bool
equals (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  x1 == x2 && y1 == y2 && z1 == z2


componentIn : Direction3d -> Vector3d -> Float
componentIn (Direction3d vector) =
  dot vector


squaredLength : Vector3d -> Float
squaredLength (Vector3d x y z) =
  x * x + y * y + z * z


length : Vector3d -> Float
length =
  squaredLength >> sqrt


normalize : Vector3d -> Maybe Vector3d
normalize vector =
  if equals zero vector then
    Nothing
  else
    Just (times (1 / length vector) vector)


direction : Vector3d -> Maybe Direction3d
direction =
  normalize >> Maybe.map Direction3d


perpendicularVector : Vector3d -> Vector3d
perpendicularVector (Vector3d x y z) =
  let
    absX =
      abs x

    absY =
      abs y

    absZ =
      abs z
  in
    if absX <= absY then
      if absX <= absZ then
        Vector3d 0 (-z) y
      else
        Vector3d (-y) x 0
    else if absY <= absZ then
      Vector3d z 0 (-x)
    else
      Vector3d (-y) x 0


normalDirection : Vector3d -> Maybe Direction3d
normalDirection =
  perpendicularVector >> direction


rotateAbout : Direction3d -> Float -> Vector3d -> Vector3d
rotateAbout (Direction3d (Vector3d dx dy dz)) angle =
  let
    halfAngle =
      0.5 * angle

    sinHalfAngle =
      sin halfAngle

    x =
      dx * sinHalfAngle

    y =
      dy * sinHalfAngle

    z =
      dz * sinHalfAngle

    w =
      cos halfAngle

    wx =
      w * x

    wy =
      w * y

    wz =
      w * z

    xx =
      x * x

    xy =
      x * y

    xz =
      x * z

    yy =
      y * y

    yz =
      y * z

    zz =
      z * z

    a00 =
      1 - 2 * (yy + zz)

    a10 =
      2 * (xy + wz)

    a20 =
      2 * (xz - wy)

    a01 =
      2 * (xy - wz)

    a11 =
      1 - 2 * (xx + zz)

    a21 =
      2 * (yz + wx)

    a02 =
      2 * (xz + wy)

    a12 =
      2 * (yz - wx)

    a22 =
      1 - 2 * (xx + yy)
  in
    \(Vector3d vx vy vz) ->
      let
        vx' =
          a00 * vx + a01 * vy + a02 * vz

        vy' =
          a10 * vx + a11 * vy + a12 * vz

        vz' =
          a20 * vx + a21 * vy + a22 * vz
      in
        Vector3d vx' vy' vz'


mirrorAlong : Direction3d -> Vector3d -> Vector3d
mirrorAlong direction =
  let
    (Direction3d (Vector3d dx dy dz)) =
      direction

    a =
      1 - 2 * dx * dx

    b =
      1 - 2 * dy * dy

    c =
      1 - 2 * dz * dz

    d =
      -2 * dy * dz

    e =
      -2 * dx * dz

    f =
      -2 * dx * dy
  in
    \(Vector3d vx vy vz) ->
      let
        vx' =
          a * vx + f * vy + e * vz

        vy' =
          f * vx + b * vy + d * vz

        vz' =
          e * vx + d * vy + c * vz
      in
        Vector3d vx' vy' vz'


relativeTo : Frame3d -> Vector3d -> Vector3d
relativeTo frame vector =
  let
    x =
      componentIn frame.xDirection vector

    y =
      componentIn frame.yDirection vector

    z =
      componentIn frame.zDirection vector
  in
    Vector3d x y z


placeIn : Frame3d -> Vector3d -> Vector3d
placeIn frame =
  let
    (Direction3d (Vector3d x1 y1 z1)) =
      frame.xDirection

    (Direction3d (Vector3d x2 y2 z2)) =
      frame.yDirection

    (Direction3d (Vector3d x3 y3 z3)) =
      frame.zDirection
  in
    \(Vector3d x y z) ->
      let
        x' =
          x1 * x + x2 * y + x3 * z

        y' =
          y1 * x + y2 * y + y3 * z

        z' =
          z1 * x + z2 * y + z3 * z
      in
        Vector3d x' y' z'


projectionIn : Direction3d -> Vector3d -> Vector3d
projectionIn ((Direction3d directionVector) as direction) vector =
  times (componentIn direction vector) directionVector


projectOnto : Plane3d -> Vector3d -> Vector3d
projectOnto plane vector =
  minus (projectionIn plane.normalDirection vector) vector


projectInto : Plane3d -> Vector3d -> Vector2d
projectInto plane vector =
  Vector2d (componentIn plane.xDirection vector) (componentIn plane.yDirection vector)


negate : Vector3d -> Vector3d
negate (Vector3d x y z) =
  Vector3d (-x) (-y) (-z)


plus : Vector3d -> Vector3d -> Vector3d
plus (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (x1 + x2) (y1 + y2) (z1 + z2)


minus : Vector3d -> Vector3d -> Vector3d
minus (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (x1 - x2) (y1 - y2) (z1 - z2)


times : Float -> Vector3d -> Vector3d
times scale (Vector3d x y z) =
  Vector3d (x * scale) (y * scale) (z * scale)


addTo : Point3d -> Vector3d -> Point3d
addTo (Point3d px py pz) (Vector3d vx vy vz) =
  Point3d (px + vx) (py + vy) (pz + vz)


subtractFrom : Point3d -> Vector3d -> Point3d
subtractFrom (Point3d px py pz) (Vector3d vx vy vz) =
  Point3d (px - vx) (py - vy) (pz - vz)


dot : Vector3d -> Vector3d -> Float
dot (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  x1 * x2 + y1 * y2 + z1 * z2


cross : Vector3d -> Vector3d -> Vector3d
cross (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
