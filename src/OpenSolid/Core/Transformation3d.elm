module OpenSolid.Core.Transformation3d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix3x3 as Matrix3x3


translationBy: Vector3d -> Transformation3d
translationBy (Vector3d vx vy vz) =
  let
    transformPoint (Point3d px py pz) = Point3d (px + vx) (py + vy) (pz + vz)
  in
    Transformation3d identity transformPoint


rotateVector: (Direction3d, Direction3d, Direction3d) -> Vector3d -> Vector3d
rotateVector (xDirection, yDirection, zDirection) =
  Matrix3x3.product xDirection yDirection zDirection


rotatePoint: Point3d -> (Direction3d, Direction3d, Direction3d) -> Point3d -> Point3d
rotatePoint (Point3d x0 y0 z0) basis (Point3d px py pz) =
  let
    (Vector3d dx dy dz) = rotateVector basis (Vector3d (px - x0) (py - y0) (pz - z0))
  in
    Point3d (x0 + dx) (y0 + dy) (z0 + dz)


rotationBasis: Direction3d -> Float -> (Direction3d, Direction3d, Direction3d)
rotationBasis (Direction3d (Vector3d vx vy vz)) angle =
  let
    halfAngle = 0.5 * angle
    sinHalfAngle = sin halfAngle
    x = vx * sinHalfAngle
    y = vy * sinHalfAngle
    z = vz * sinHalfAngle
    w = cos halfAngle
    wx = w * x
    wy = w * y
    wz = w * z
    xx = x * x
    xy = x * y
    xz = x * z
    yy = y * y
    yz = y * z
    zz = z * z
    xDirection = Direction3d (Vector3d (1 - 2 * (yy + zz)) (2 * (xy + wz)) (2 * (xz - wy)))
    yDirection = Direction3d (Vector3d (2 * (xy - wz)) (1 - 2 * (xx + zz)) (2 * (yz + wx)))
    zDirection = Direction3d (Vector3d (2 * (xz + wy)) (2 * (yz - wx)) (1 - 2 * (xx + yy)))
  in
    (xDirection, yDirection, zDirection)


rotationAbout: Axis3d -> Float -> Transformation3d
rotationAbout axis angle =
  let
    basis = rotationBasis axis.direction angle
  in
    Transformation3d (rotateVector basis) (rotatePoint axis.originPoint basis)


localizationTo: Frame3d -> Transformation3d
localizationTo frame =
  let
    transformVector = Matrix3x3.dotProduct frame.xDirection frame.yDirection frame.zDirection
    (Point3d x0 y0 z0) = frame.originPoint
    transformPoint (Point3d px py pz) =
      let
        (Vector3d x y z) = transformVector (Vector3d (px - x0) (py - y0) (pz - z0))
      in
        Point3d x y z
  in
    Transformation3d transformVector transformPoint


globalizationFrom: Frame3d -> Transformation3d
globalizationFrom frame =
  let
    transformVector = Matrix3x3.product frame.xDirection frame.yDirection frame.zDirection
    (Point3d x0 y0 z0) = frame.originPoint
    transformPoint (Point3d px py pz) =
      let
        (Vector3d dx dy dz) = transformVector (Vector3d px py pz)
      in
        Point3d (x0 + dx) (y0 + dy) (z0 + dz)
  in
    Transformation3d transformVector transformPoint


andThen: Transformation3d -> Transformation3d -> Transformation3d
andThen second first =
  let
    transformVector = first.transformVector >> second.transformVector
    transformPoint = first.transformPoint >> second.transformPoint
  in
    Transformation3d transformVector transformPoint
