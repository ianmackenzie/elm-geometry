--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module PolygonTriangulation exposing (main)

import Array exposing (Array)
import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import Browser
import DelaunayTriangulation2d
import Html exposing (Html)
import Point2d exposing (Point2d)
import Random
import Task exposing (Task)
import Time


testPoints : Array Point2d
testPoints =
    let
        pointGenerator =
            Random.map2
                (\x y -> Point2d.fromCoordinates ( x, y ))
                (Random.float 0 100)
                (Random.float 0 100)

        arrayGenerator =
            Random.list 200 pointGenerator
    in
    Random.step arrayGenerator (Random.initialSeed 1)
        |> Tuple.first
        |> Array.fromList


triangulateNTimes : Int -> ()
triangulateNTimes count =
    if count > 0 then
        let
            _ =
                DelaunayTriangulation2d.fromPoints testPoints
        in
        triangulateNTimes (count - 1)

    else
        ()


task : Task x Float
task =
    Time.now
        |> Task.andThen
            (\startTime ->
                let
                    _ =
                        triangulateNTimes 100
                in
                Time.now
                    |> Task.map
                        (\endTime ->
                            toFloat (Time.posixToMillis endTime - Time.posixToMillis startTime) / 1000
                        )
            )


suite : Benchmark
suite =
    Benchmark.benchmark "Delaunay triangulation"
        (\() -> DelaunayTriangulation2d.fromPoints testPoints)


benchmarkProgram : BenchmarkProgram
benchmarkProgram =
    Benchmark.Runner.program suite


type TaskModel
    = Running
    | Complete Float


taskView : TaskModel -> Browser.Document msg
taskView taskModel =
    { title = "Delaunay Triangulation Benchmark"
    , body =
        [ Html.text <|
            case taskModel of
                Running ->
                    "Running"

                Complete time ->
                    "Elapsed: " ++ String.fromFloat time
        ]
    }


type alias TaskProgram =
    Program () TaskModel Float


taskProgram : TaskProgram
taskProgram =
    Browser.document
        { init = \() -> ( Running, Task.perform identity task )
        , view = taskView
        , update = \value model -> ( Complete value, Cmd.none )
        , subscriptions = always Sub.none
        }


main : TaskProgram
main =
    taskProgram
