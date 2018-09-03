--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


port module Main exposing (main)

import Json.Encode exposing (Value)
import Regex exposing (Regex)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions
import Script.Shell as Shell exposing (Shell)


svgRegex : Regex
svgRegex =
    Regex.regex "<svg.*</svg>"


handleError : Script x a -> Script Int a
handleError =
    Script.onError printError


printError : x -> Script Int a
printError error =
    Script.printLine ("ERROR: " ++ toString error)
        |> Script.andThen (\() -> Script.fail 1)


convertToSvgDocument : String -> String
convertToSvgDocument svg =
    let
        xmlElement =
            "<?xml version=\"1.0\" standalone=\"no\"?>"

        docType =
            "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

        regex =
            Regex.regex "<svg([^>]*)>"
    in
    xmlElement
        ++ docType
        ++ Regex.replace (Regex.AtMost 1)
            regex
            (\{ submatches } ->
                "<svg"
                    ++ String.concat (List.filterMap identity submatches)
                    ++ " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
                    ++ "<defs xmlns=\"http://www.w3.org/1999/xhtml\"><style type=\"text/css\"><![CDATA[\n html, head, body { padding: 0px; margin: 0px; }\nbody { font-family: calibri, helvetica, arial, sans-serif; }]]></style></defs>"
            )
            svg


extractSvg : Shell -> String -> Script Int String
extractSvg shell path =
    let
        tempFileName =
            "tmp.html"
    in
    Shell.execute "cd" shell
        |> handleError
        |> Script.map
            (\currentDirectory ->
                String.concat
                    [ "file:///"
                    , currentDirectory
                        |> String.trim
                        |> Regex.replace Regex.All
                            (Regex.regex "\\\\")
                            (always "/")
                    , "/"
                    , tempFileName
                    ]
            )
        |> Script.andThen
            (\url ->
                Shell.execute ("elm-make " ++ path ++ " --output " ++ tempFileName) shell
                    |> handleError
                    |> Script.andThen
                        (\_ ->
                            let
                                command =
                                    String.join " "
                                        [ "\"C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe\""
                                        , "--headless"
                                        , "--disable-gpu"
                                        , "--virtual-time-budget=1000"
                                        , "--dump-dom"
                                        , url
                                        ]
                            in
                            Shell.execute command shell |> handleError
                        )
            )
        |> Script.map
            (\htmlString ->
                Regex.find Regex.All svgRegex htmlString
                    |> List.map .match
                    |> String.concat
                    |> convertToSvgDocument
            )
        |> Script.aside
            (\_ ->
                Shell.execute "del tmp.html" shell
                    |> handleError
                    |> Script.ignore
            )


script : Script.Context -> Script Int ()
script { shell, fileSystem } =
    let
        outputRoot =
            FileSystem.directory Permissions.readWrite
                "../../opensolid.github.io/geometry/2.1.0/"
                fileSystem

        sourcesAndDestinations =
            [ ( "DefaultParameterization.elm"
              , "default-parameterization.svg"
              )
            , ( "ArcLengthParameterization.elm"
              , "arc-length-parameterization.svg"
              )
            ]
    in
    sourcesAndDestinations
        |> Script.forEach
            (\( source, destination ) ->
                extractSvg shell ("src/ReleaseNotes/" ++ source)
                    |> Script.andThen
                        (\svg ->
                            let
                                outputFile =
                                    Directory.file
                                        ("release-notes/" ++ destination)
                                        outputRoot
                            in
                            File.writeTo outputFile svg
                                |> handleError
                        )
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
