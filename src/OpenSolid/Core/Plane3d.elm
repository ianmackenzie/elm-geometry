module Plane3d
  ( xy
  , xz
  , yx
  , yz
  , zx
  , zy
  , fromPointAndBasis
  , fromPointAndNormal
  , offsetBy
  , flipped
  , normalAxis
  , transformedBy
  ) where


import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Direction3d as Direction3d


xy: Plane3d
xy =
  Plane3d Point3d.origin Direction3d.x Direction3d.y Direction3d.z


xz: Plane3d
xz =
  Plane3d Point3d.origin Direction3d.x Direction3d.z (Direction3d.negated Direction3d.y)


yx: Plane3d
yx =
  Plane3d Point3d.origin Direction3d.y Direction3d.x (Direction3d.negated Direction3d.z)


yz: Plane3d
yz =
  Plane3d Point3d.origin Direction3d.y Direction3d.z Direction3d.x


zx: Plane3d
zx =
  Plane3d Point3d.origin Direction3d.z Direction3d.x Direction3d.y


zy: Plane3d
zy =
  Plane3d Point3d.origin Direction3d.z Direction3d.y (Direction3d.negated Direction3d.x)


fromPointAndBasis: Point3d -> Direction3d -> Direction3d -> Plane3d
fromPointAndBasis originPoint xDirection yDirection =
  Plane3d originPoint xDirection yDirection (Vector3d.cross yDirection xDirection)


fromPointAndNormal: Point3d -> Direction3d -> Plane3d
fromPointAndNormal originPoint normalDirection =
  let
    xDirection = Direction3d.normalDirection normalDirection
    yDirection = Vector3d.cross xDirection normalDirection
  in
    Plane3d originPoint xDirection yDirection normalDirection


offsetBy: Float -> Plane3d -> Plane3d
offsetBy distance plane =
  let
    displacement = Direction2d.times distance plane.normalDirection
  in
    { plane | originPoint = Point2d.plus displacement plane.originPoint }


flipped: Plane3d -> Plane3d
flipped plane =
  { plane | normalDirection = Direction3d.reversed plane.normalDirection }


normalAxis: Plane3d -> Axis3d
normalAxis plane =
  Axis3d plane.originPoint plane.normalDirection


transformedBy: Transformation3d -> Plane3d -> Plane3d
transformedBy transformation plane =
  let
    originPoint = Point3d.transformedBy transformation plane.originPoint
    transformedDirection = Direction3d.transformedBy transformation
    xDirection = transformedDirection plane.xDirection
    yDirection = transformedDirection plane.yDirection
    normalDirection = transformedDirection plane.normalDirection
  in
    Plane3d originPoint xDirection yDirection normalDirection
