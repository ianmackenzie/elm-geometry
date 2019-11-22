--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polyline3d exposing
    ( Polyline3d
    , fromVertices, on
    , vertices, segments, length, boundingBox, centroid
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices
    , at, at_
    , relativeTo, placeIn, projectInto
    )

{-| A `Polyline3d` represents a sequence of vertices in 3D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline3d


# Constructors

@docs fromVertices, on


# Properties

@docs vertices, segments, length, boundingBox, centroid


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity, Rate)
import Quantity.Extra as Quantity
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Polyline3d units coordinates =
    Types.Polyline3d units coordinates


{-| Construct a polyline from its vertices:

    examplePolyline =
        Polyline3d.fromVertices
            [ Point3d.meters 0 0 0
            , Point3d.meters 1 0 0
            , Point3d.meters 1 2 0
            , Point3d.meters 1 2 3
            ]

-}
fromVertices : List (Point3d units coordinates) -> Polyline3d units coordinates
fromVertices givenVertices =
    Types.Polyline3d givenVertices


{-| Construct a 3D polyline lying _on_ a sketch plane by providing a 2D polyline
specified in XY coordinates _within_ the sketch plane.

    Polyline3d.on SketchPlane3d.yz <|
        Polyline2d.fromVertices
            [ Point2d.meters 0 0
            , Point2d.meters 1 0
            , Point2d.meters 1 1
            , Point2d.meters 2 1
            ]
    --> Polyline3d.fromVertices
    -->     [ Point3d.meters 0 0 0
    -->     , Point3d.meters 0 1 0
    -->     , Point3d.meters 0 1 1
    -->     , Point3d.meters 0 2 1
    -->     ]

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Polyline2d units coordinates2d -> Polyline3d units coordinates3d
on sketchPlane polyline2d =
    Polyline2d.vertices polyline2d
        |> List.map (Point3d.on sketchPlane)
        |> fromVertices


{-| Convert a polyline from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Polyline3d units1 coordinates -> Polyline3d units2 coordinates
at rate (Types.Polyline3d polylineVertices) =
    Types.Polyline3d (List.map (Point3d.at rate) polylineVertices)


{-| Convert a polyline from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Polyline3d units1 coordinates -> Polyline3d units2 coordinates
at_ rate polyline =
    at (Quantity.inverse rate) polyline


{-| Get the vertices of a polyline.
-}
vertices : Polyline3d units coordinates -> List (Point3d units coordinates)
vertices (Types.Polyline3d polylineVertices) =
    polylineVertices


{-| Get the individual segments of a polyline.

    Polyline3d.segments examplePolyline
    --> [ LineSegment3d.from
    -->     (Point3d.meters 0 0 0)
    -->     (Point3d.meters 1 0 0)
    --> , LineSegment3d.from
    -->     (Point3d.meters 1 0 0)
    -->     (Point3d.meters 1 2 0)
    --> , LineSegment3d.from
    -->     (Point3d.meters 1 2 0)
    -->     (Point3d.meters 1 2 3)
    --> ]

-}
segments : Polyline3d units coordinates -> List (LineSegment3d units coordinates)
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment3d.from all rest


{-| Get the overall length of a polyline (the sum of the lengths of its
segments).

    Polyline3d.length examplePolyline
    --> Length.meters 6

-}
length : Polyline3d units coordinates -> Quantity Float units
length polyline =
    segments polyline |> List.map LineSegment3d.length |> Quantity.sum


{-| Scale a polyline about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> Polyline3d units coordinates -> Polyline3d units coordinates
scaleAbout point scale polyline =
    mapVertices (Point3d.scaleAbout point scale) polyline


{-| Rotate a polyline around the given axis by the given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> Polyline3d units coordinates -> Polyline3d units coordinates
rotateAround axis angle polyline =
    mapVertices (Point3d.rotateAround axis angle) polyline


{-| Translate a polyline by the given displacement.
-}
translateBy : Vector3d units coordinates -> Polyline3d units coordinates -> Polyline3d units coordinates
translateBy vector polyline =
    mapVertices (Point3d.translateBy vector) polyline


{-| Translate a polyline in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Polyline3d units coordinates -> Polyline3d units coordinates
translateIn direction distance polyline =
    translateBy (Vector3d.withLength distance direction) polyline


{-| Mirror a polyline across the given plane.
-}
mirrorAcross : Plane3d units coordinates -> Polyline3d units coordinates -> Polyline3d units coordinates
mirrorAcross plane polyline =
    mapVertices (Point3d.mirrorAcross plane) polyline


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a polyline onto a plane. This will flatten the polyline.
-}
projectOnto : Plane3d units coordinates -> Polyline3d units coordinates -> Polyline3d units coordinates
projectOnto plane polyline =
    mapVertices (Point3d.projectOnto plane) polyline


{-| Transform each vertex of a polyline by the given function. All other
transformations can be defined in terms of `mapVertices`; for example,

    Polyline3d.mirrorAcross plane

is equivalent to

    Polyline3d.mapVertices (Point3d.mirrorAcross plane)

-}
mapVertices : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> Polyline3d units1 coordinates1 -> Polyline3d units2 coordinates2
mapVertices function polyline =
    vertices polyline |> List.map function |> fromVertices


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Polyline3d units globalCoordinates -> Polyline3d units localCoordinates
relativeTo frame polyline =
    mapVertices (Point3d.relativeTo frame) polyline


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Polyline3d units localCoordinates -> Polyline3d units globalCoordinates
placeIn frame polyline =
    mapVertices (Point3d.placeIn frame) polyline


{-| Project a polyline into a given sketch plane.
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Polyline3d units coordinates3d -> Polyline2d units coordinates2d
projectInto sketchPlane polyline =
    vertices polyline
        |> List.map (Point3d.projectInto sketchPlane)
        |> Polyline2d.fromVertices


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline3d.boundingBox examplePolyline
    --> Just <|
    -->     BoundingBox3d.from
    -->         (Point3d.meters 0 0 0)
    -->         (Point3d.meters 1 2 3)

-}
boundingBox : Polyline3d units coordinates -> Maybe (BoundingBox3d units coordinates)
boundingBox polyline =
    BoundingBox3d.hullN (vertices polyline)


{-| Find the centroid (center of mass) of a polyline. This is the
length-weighted average of the edges of the polyline, _not_ the centroid of its
vertices. Returns `Nothing` if the polyline is empty (has no vertices).

    Polyline3d.centroid examplePolyline
    --> Just (Point3d.meters 0.9167 1.333 0.75)

-}
centroid : Polyline3d units coordinates -> Maybe (Point3d units coordinates)
centroid polyline =
    case ( vertices polyline, boundingBox polyline ) of
        ( [], _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( first :: _, Just box ) ->
            let
                polylineLength =
                    length polyline
            in
            if polylineLength == Quantity.zero then
                Just first

            else
                let
                    roughCentroid =
                        BoundingBox3d.centerPoint box

                    helper =
                        refineBySegment polylineLength roughCentroid
                in
                segments polyline
                    |> List.foldl helper roughCentroid
                    |> Just


refineBySegment : Quantity Float units -> Point3d units coordinates -> LineSegment3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
refineBySegment polylineLength roughCentroid segment currentCentroid =
    let
        segmentMidpoint =
            LineSegment3d.midpoint segment

        segmentLength =
            LineSegment3d.length segment

        offset =
            Vector3d.from roughCentroid segmentMidpoint
                |> Vector3d.scaleBy (Quantity.ratio segmentLength polylineLength)
    in
    currentCentroid |> Point3d.translateBy offset
