port module Main exposing (main)

{-| Format all elm modules in the source directories your project
-}

import Ansi.Color
import Ansi.Font
import Bytes exposing (Bytes)
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
    | SingleRun StateSingleRun
    | Watching WatchState
    | ElmJsonReadFailed String


type StateSingleRun
    = SingleRunRunning SingleRunRunningState
    | SingleRunSourceDirectoryReadFailed { path : String, message : String }
    | SingleRunSourceFileReadFailed { path : String, message : String }


type alias SingleRunRunningState =
    { elmJsonSourceDirectories : List String
    , sourceDirectoriesToRead : Set String
    , sourceFilesToRead : Set String
    , formattedModulesToWrite : List { path : String, content : Bytes }
    }


type alias WatchState =
    { elmJsonSourceDirectories : List String
    , sourceFilesToRead : Set String
    , formattedModulesToWrite :
        List
            { path : String
            , content : Bytes
            }
    }


type Mode
    = ModeSingleRun
    | ModeWatch


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
                                WaitingForElmJson { mode = ModeSingleRun }

                            [ "watch" ] ->
                                WaitingForElmJson { mode = ModeWatch }

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
                                                case waitingForElmJson.mode of
                                                    ModeSingleRun ->
                                                        SingleRun
                                                            (SingleRunRunning
                                                                { elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                                                , sourceDirectoriesToRead =
                                                                    computedElmJsonSourceDirectories |> Set.fromList
                                                                , sourceFilesToRead = Set.empty
                                                                , formattedModulesToWrite = []
                                                                }
                                                            )

                                                    ModeWatch ->
                                                        Watching
                                                            { elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                                            , sourceFilesToRead = Set.empty
                                                            , formattedModulesToWrite = []
                                                            }
                    )

        SingleRun singleRunState ->
            singleRunInterface singleRunState

        Watching watchState ->
            watchInterface watchState

        ElmJsonReadFailed elmJsonDecodeError ->
            badExitWith elmJsonDecodeError


singleRunInterface : StateSingleRun -> Node.Interface State
singleRunInterface singleRunState =
    case singleRunState of
        SingleRunRunning running ->
            singleRunRunningInterface running

        SingleRunSourceDirectoryReadFailed directoryReadError ->
            badExitWith
                ("failed to check the source directory "
                    ++ directoryReadError.path
                    ++ " because "
                    ++ directoryReadError.message
                )

        SingleRunSourceFileReadFailed fileReadError ->
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


singleRunRunningInterface : SingleRunRunningState -> Node.Interface State
singleRunRunningInterface running =
    [ running.formattedModulesToWrite
        |> List.map
            (\moduleToFormat ->
                Node.fileWrite
                    { path = moduleToFormat.path
                    , content = moduleToFormat.content
                    }
             {- TODO
                |> Node.interfaceFutureMap (\() ->
                  Running
                    { elmJsonSourceDirectories = running.elmJsonSourceDirectories
                    , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                    , sourceFilesToRead = running.sourceFilesToRead
                    , formattedModulesToWrite =
                         running.formattedModulesToWrite |> Dict.remove moduleToFormat
                    }
                )
             -}
            )
        |> Node.interfaceBatch
    , running.sourceDirectoriesToRead
        |> Set.toList
        |> List.map
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err directorySourcePathSubPathRequestError ->
                                    SingleRun
                                        (SingleRunSourceDirectoryReadFailed
                                            { path = sourceDirectoryPath
                                            , message = directorySourcePathSubPathRequestError.message
                                            }
                                        )

                                Ok subPaths ->
                                    SingleRun
                                        (SingleRunRunning
                                            { elmJsonSourceDirectories = running.elmJsonSourceDirectories
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
                                            , formattedModulesToWrite = running.formattedModulesToWrite
                                            }
                                        )
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
                                    SingleRun
                                        (SingleRunSourceFileReadFailed
                                            { path = sourceFilePath
                                            , message = sourceFileReadError.message
                                            }
                                        )

                                Ok sourceBytes ->
                                    case sourceBytes |> bytesToElmSyntax of
                                        Err message ->
                                            SingleRun
                                                (SingleRunSourceFileReadFailed
                                                    { path = sourceFilePath
                                                    , message = message
                                                    }
                                                )

                                        Ok syntax ->
                                            SingleRun
                                                (SingleRunRunning
                                                    { elmJsonSourceDirectories = running.elmJsonSourceDirectories
                                                    , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                                                    , sourceFilesToRead =
                                                        running.sourceFilesToRead
                                                            |> Set.remove sourceFilePath
                                                    , formattedModulesToWrite =
                                                        { path = sourceFilePath
                                                        , content =
                                                            syntax
                                                                |> ElmSyntaxPrint.module_
                                                                |> ElmSyntaxPrint.toString
                                                                |> Bytes.Encode.string
                                                                |> Bytes.Encode.encode
                                                        }
                                                            :: running.formattedModulesToWrite
                                                    }
                                                )
                        )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


bytesToElmSyntax : Bytes -> Result String Elm.Syntax.File.File
bytesToElmSyntax sourceBytes =
    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
        Nothing ->
            Err "source bytes couldn't be decoded into UTF-8"

        Just source ->
            case source |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ of
                Nothing ->
                    Err "source couldn't be parsed. Check for compiler errors and try again."

                Just syntax ->
                    Ok syntax


watchInterface : WatchState -> Node.Interface State
watchInterface watching =
    [ watching.elmJsonSourceDirectories
        |> List.map
            (\elmJsonSourceDirectory ->
                Node.fileChangeListen elmJsonSourceDirectory
                    |> Node.interfaceFutureMap
                        (\fileChange ->
                            case fileChange of
                                Node.FileRemoved removedSubPath ->
                                    Watching watching

                                Node.FileAddedOrChanged addedOrChangedPath ->
                                    if addedOrChangedPath |> String.endsWith ".elm" then
                                        Watching
                                            { elmJsonSourceDirectories = watching.elmJsonSourceDirectories
                                            , sourceFilesToRead =
                                                watching.sourceFilesToRead |> Set.insert addedOrChangedPath
                                            , formattedModulesToWrite = watching.formattedModulesToWrite
                                            }

                                    else
                                        Watching watching
                        )
            )
        |> Node.interfaceBatch
    , watching.formattedModulesToWrite
        |> List.map
            (\moduleToFormat ->
                Node.fileWrite
                    { path = moduleToFormat.path
                    , content = moduleToFormat.content
                    }
             {- TODO
                |> Node.interfaceFutureMap (\() ->
                  Running
                    { elmJsonSourceDirectories = running.elmJsonSourceDirectories
                    , sourceFilesToRead = running.sourceFilesToRead
                    , formattedModulesToWrite =
                         running.formattedModulesToWrite |> Dict.remove moduleToFormat
                    }
                )
             -}
            )
        |> Node.interfaceBatch
    , watching.sourceFilesToRead
        |> Set.toList
        |> List.map
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            case sourceBytesOrError of
                                Err sourceFileReadError ->
                                    -- maybe add a note to display?
                                    Watching watching

                                Ok sourceBytes ->
                                    case sourceBytes |> bytesToElmSyntax of
                                        Err _ ->
                                            -- maybe add a note to display?
                                            Watching watching

                                        Ok syntax ->
                                            Watching
                                                { elmJsonSourceDirectories = watching.elmJsonSourceDirectories
                                                , sourceFilesToRead =
                                                    watching.sourceFilesToRead
                                                        |> Set.remove sourceFilePath
                                                , formattedModulesToWrite =
                                                    { path = sourceFilePath
                                                    , content =
                                                        syntax
                                                            |> ElmSyntaxPrint.module_
                                                            |> ElmSyntaxPrint.toString
                                                            |> Bytes.Encode.string
                                                            |> Bytes.Encode.encode
                                                    }
                                                        :: watching.formattedModulesToWrite
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
