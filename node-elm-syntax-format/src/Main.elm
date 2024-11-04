port module Main exposing (main)

{-| Format all elm modules in the source directories your project
-}

import Ansi.Color
import Ansi.Font
import Bytes
import Bytes.Decode
import Bytes.Encode
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Type
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import Json.Decode
import Json.Encode
import Node
import Set exposing (Set)


type State
    = WaitingForLaunchArguments
    | ShowingHelp
    | WaitingForElmJson { mode : Mode }
    | Running RunningState
    | ElmJsonReadFailed String
    | SourceDirectoryReadFailed { path : String, message : String }
    | SourceFileReadFailed { path : String, message : String }


type alias RunningState =
    { mode : Mode
    , elmJsonSourceDirectories : List String
    , sourceDirectoriesToRead : Set String
    , sourceFilesToRead : Set String
    , modulesToFormat :
        List
            { path : String
            , syntax : Elm.Syntax.File.File
            }
    }


type Mode
    = SingleRun
    | Watch


initialState : State
initialState =
    WaitingForLaunchArguments


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForLaunchArguments ->
            Node.launchArgumentsRequest
                |> Node.interfaceFutureMap
                    (\launchArguments ->
                        case launchArguments |> List.drop 2 of
                            [] ->
                                WaitingForElmJson { mode = SingleRun }

                            [ "watch" ] ->
                                WaitingForElmJson { mode = Watch }

                            [ "help" ] ->
                                ShowingHelp

                            [ "--help" ] ->
                                ShowingHelp

                            [ "-h" ] ->
                                ShowingHelp

                            _ ->
                                ShowingHelp
                    )

        ShowingHelp ->
            Node.standardOutWrite
                ("""Format elm 0.19 """
                    ++ ("source directory" |> Ansi.Font.bold)
                    ++ """ modules like elm-format (https://github.com/avh4/elm-format).

"""
                    ++ ([ { command = "elm-syntax-format"
                          , description = "format all of them once"
                          }
                        , { command = "elm-syntax-format watch"
                          , description = "format edited and added modules on save"
                          }
                        ]
                            |> List.map
                                (\commandAndDescription ->
                                    "  - "
                                        ++ (commandAndDescription.command
                                                |> Ansi.Color.fontColor Ansi.Color.cyan
                                           )
                                        ++ ": "
                                        ++ commandAndDescription.description
                                )
                            |> String.join "\n"
                       )
                    ++ """

"""
                )

        WaitingForElmJson waitingForElmJson ->
            Node.fileRequest "elm.json"
                |> Node.interfaceFutureMap
                    (\elmJsonBytesOrError ->
                        case elmJsonBytesOrError of
                            Err fileReadError ->
                                ElmJsonReadFailed
                                    ("elm.json couldn't be read because "
                                        ++ fileReadError.message
                                        ++ " (code "
                                        ++ fileReadError.code
                                        ++ ")"
                                    )

                            Ok elmJsonBytes ->
                                case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                                    Nothing ->
                                        ElmJsonReadFailed "elm.json bytes could not be decoded into UTF-8 String"

                                    Just elmJsonString ->
                                        case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                            Err jsonDecodeError ->
                                                ElmJsonReadFailed
                                                    ("elm.json failed to parse due to "
                                                        ++ (jsonDecodeError |> Json.Decode.errorToString)
                                                    )

                                            Ok elmJson ->
                                                let
                                                    computedElmJsonSourceDirectories : List String
                                                    computedElmJsonSourceDirectories =
                                                        elmJson |> elmJsonSourceDirectories
                                                in
                                                Running
                                                    { mode = waitingForElmJson.mode
                                                    , elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                                    , sourceDirectoriesToRead =
                                                        computedElmJsonSourceDirectories |> Set.fromList
                                                    , sourceFilesToRead = Set.empty
                                                    , modulesToFormat = []
                                                    }
                    )

        Running running ->
            runningInterface running

        ElmJsonReadFailed elmJsonDecodeError ->
            badExitWith elmJsonDecodeError

        SourceDirectoryReadFailed directoryReadError ->
            badExitWith
                ("failed to check the source directory "
                    ++ directoryReadError.path
                    ++ " because "
                    ++ directoryReadError.message
                )

        SourceFileReadFailed fileReadError ->
            badExitWith
                ("failed to read the source file "
                    ++ fileReadError.path
                    ++ " because "
                    ++ fileReadError.message
                )


badExitWith : String -> Node.Interface future_
badExitWith errorMessage =
    [ Node.standardErrWrite (errorMessage ++ "\n")
    , Node.exit 1
    ]
        |> Node.interfaceBatch


runningInterface : RunningState -> Node.Interface State
runningInterface running =
    [ case running.mode of
        SingleRun ->
            Node.interfaceNone

        Watch ->
            running.elmJsonSourceDirectories
                |> List.map
                    (\elmJsonSourceDirectory ->
                        Node.fileChangeListen elmJsonSourceDirectory
                            |> Node.interfaceFutureMap
                                (\fileChange ->
                                    case fileChange of
                                        Node.FileRemoved removedSubPath ->
                                            Running running

                                        Node.FileAddedOrChanged addedOrChangedPath ->
                                            if addedOrChangedPath |> String.endsWith ".elm" then
                                                Running
                                                    { mode = running.mode
                                                    , elmJsonSourceDirectories = running.elmJsonSourceDirectories
                                                    , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                                                    , sourceFilesToRead =
                                                        running.sourceFilesToRead |> Set.insert addedOrChangedPath
                                                    , modulesToFormat = running.modulesToFormat
                                                    }

                                            else
                                                Running running
                                )
                    )
                |> Node.interfaceBatch
    , if (running.sourceDirectoriesToRead |> Set.isEmpty) && (running.sourceFilesToRead |> Set.isEmpty) then
        running.modulesToFormat
            |> List.map
                (\moduleToFormat ->
                    Node.fileWrite
                        { path = moduleToFormat.path
                        , content =
                            moduleToFormat.syntax
                                |> ElmSyntaxPrint.module_
                                |> ElmSyntaxPrint.toString
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                        }
                )
            |> Node.interfaceBatch

      else
        Node.interfaceNone
    , running.sourceDirectoriesToRead
        |> Set.toList
        |> List.map
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err directorySourcePathSubPathRequestError ->
                                    SourceDirectoryReadFailed
                                        { path = sourceDirectoryPath
                                        , message = directorySourcePathSubPathRequestError.message
                                        }

                                Ok subPaths ->
                                    Running
                                        { mode = running.mode
                                        , elmJsonSourceDirectories = running.elmJsonSourceDirectories
                                        , sourceDirectoriesToRead =
                                            running.sourceDirectoriesToRead
                                                |> Set.remove sourceDirectoryPath
                                        , sourceFilesToRead =
                                            subPaths
                                                |> List.foldl
                                                    (\subPath soFar ->
                                                        if subPath |> String.endsWith ".elm" then
                                                            soFar |> Set.insert (sourceDirectoryPath ++ "/" ++ subPath)

                                                        else
                                                            soFar
                                                    )
                                                    running.sourceFilesToRead
                                        , modulesToFormat = running.modulesToFormat
                                        }
                        )
            )
        |> Node.interfaceBatch
    , running.sourceFilesToRead
        |> Set.toList
        |> List.map
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            case sourceBytesOrError of
                                Err sourceFileReadError ->
                                    SourceFileReadFailed
                                        { path = sourceFilePath
                                        , message = sourceFileReadError.message
                                        }

                                Ok sourceBytes ->
                                    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
                                        Nothing ->
                                            SourceFileReadFailed
                                                { path = sourceFilePath
                                                , message = "source bytes couldn't be decoded into UTF-8"
                                                }

                                        Just source ->
                                            case source |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ of
                                                Nothing ->
                                                    SourceFileReadFailed
                                                        { path = sourceFilePath
                                                        , message = "source couldn't be parsed (by elm-syntax). Check for compiler errors and try again."
                                                        }

                                                Just syntax ->
                                                    Running
                                                        { mode = running.mode
                                                        , elmJsonSourceDirectories = running.elmJsonSourceDirectories
                                                        , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                                                        , sourceFilesToRead =
                                                            running.sourceFilesToRead
                                                                |> Set.remove sourceFilePath
                                                        , modulesToFormat =
                                                            { path = sourceFilePath, syntax = syntax }
                                                                :: running.modulesToFormat
                                                        }
                        )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


elmJsonSourceDirectories : Elm.Project.Project -> List String
elmJsonSourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs

        Elm.Project.Package _ ->
            [ "src" ]


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
