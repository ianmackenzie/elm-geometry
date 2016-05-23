{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module VectorTests exposing (vector2d, vector3d, vectorTests)

import ElmTest exposing (..)
import Check exposing (..)
import Check.Producer as Producer exposing (Producer)
import Check.Test exposing (evidenceToTest)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d


component : Producer Float
component =
    Producer.rangeFloat -10 10


vector2d : Producer Vector2d
vector2d =
    let
        componentsProducer =
            Producer.tuple ( component, component )
    in
        Producer.convert Vector2d.fromComponents
            Vector2d.components
            componentsProducer


vector3d : Producer Vector3d
vector3d =
    let
        componentsProducer =
            Producer.tuple3 ( component, component, component )
    in
        Producer.convert Vector3d.fromComponents
            Vector3d.components
            componentsProducer


smallVector2d : Producer Vector2d
smallVector2d =
    let
        scaledProducer =
            Producer.convert (Vector2d.times 0.2)
                (Vector2d.times 5)
                vector2d
    in
        Producer.filter (\v -> Vector2d.length v > 0.1) scaledProducer


smallVector3d : Producer Vector3d
smallVector3d =
    let
        scaledProducer =
            Producer.convert (Vector3d.times 0.2)
                (Vector3d.times 5)
                vector3d
    in
        Producer.filter (\v -> Vector3d.length v > 0.1) scaledProducer


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        Just _ ->
            False


isApproximatelyZero : Float -> Bool
isApproximatelyZero value =
    abs value < 1.0e-12


normalizeZero2d : Test
normalizeZero2d =
    test "Normalized zero vector in 2D"
        (assert (isNothing (Vector2d.normalize Vector2d.zero)))


normalizeZero3d : Test
normalizeZero3d =
    test "Normalized zero vector in 3D"
        (assert (isNothing (Vector3d.normalize Vector3d.zero)))


normalize2d : Claim
normalize2d =
    let
        predicate vector =
            case (Vector2d.normalize vector) of
                Nothing ->
                    False

                Just normalized ->
                    isApproximatelyZero (Vector2d.length normalized - 1)
    in
        claim "Normalizing vector in 2D gives length of approximately 1"
            `true` predicate
            `for` smallVector2d


normalize3d : Claim
normalize3d =
    let
        predicate vector =
            case (Vector3d.normalize vector) of
                Nothing ->
                    False

                Just normalized ->
                    isApproximatelyZero (Vector3d.length normalized - 1)
    in
        claim "Normalizing vector in 3D gives length of approximately 1"
            `true` predicate
            `for` smallVector3d


vectorTests : Test
vectorTests =
    ElmTest.suite "Vector tests"
        [ normalizeZero2d
        , normalizeZero3d
        , evidenceToTest (quickCheck normalize2d)
        , evidenceToTest (quickCheck normalize3d)
        ]
