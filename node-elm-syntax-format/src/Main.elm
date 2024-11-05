port module Main exposing (main)

{-| Format all elm modules in the source directories your project
-}

import Ansi.Color
import Ansi.Cursor
import Ansi.Font
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
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
    | SingleRun SingleRunRunningState
    | Watching WatchState
    | ElmJsonReadFailed String


type alias SingleRunRunningState =
    { elmJsonSourceDirectories : List String
    , sourceDirectoriesToRead : Set String
    , sourceFilesToRead : Set String
    , formattedModulesToWrite : Dict String Bytes
    , sourceDirectoryReadErrors : List { path : String, message : String }
    , sourceFileReadErrors : List { path : String, message : String }
    }


type alias WatchState =
    { elmJsonSourceDirectories : List String
    , sourceFilesToRead : Set String
    , formattedModulesToWrite : Dict String Bytes
    , sourceFileReadErrors : Dict String String
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
                                                            { elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                                            , sourceDirectoriesToRead =
                                                                computedElmJsonSourceDirectories |> Set.fromList
                                                            , sourceFilesToRead = Set.empty
                                                            , formattedModulesToWrite = Dict.empty
                                                            , sourceFileReadErrors = []
                                                            , sourceDirectoryReadErrors = []
                                                            }

                                                    ModeWatch ->
                                                        Watching
                                                            { elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                                            , sourceFilesToRead = Set.empty
                                                            , formattedModulesToWrite = Dict.empty
                                                            , sourceFileReadErrors = Dict.empty
                                                            }
                    )

        SingleRun singleRunState ->
            singleRunInterface singleRunState

        Watching watchState ->
            watchInterface watchState

        ElmJsonReadFailed elmJsonDecodeError ->
            [ Node.standardErrWrite (elmJsonDecodeError ++ "\n")
            , Node.exit 1
            ]
                |> Node.interfaceBatch


singleRunInterface : SingleRunRunningState -> Node.Interface State
singleRunInterface state =
    [ state.formattedModulesToWrite
        |> Dict.toList
        |> List.map
            (\( moduleToFormatPath, moduleToFormatContent ) ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\result ->
                            SingleRun
                                { state
                                    | formattedModulesToWrite =
                                        state.formattedModulesToWrite |> Dict.remove moduleToFormatPath
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoriesToRead
        |> Set.toList
        |> List.map
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err directorySourcePathSubPathRequestError ->
                                    SingleRun
                                        { state
                                            | sourceDirectoryReadErrors =
                                                { path = sourceDirectoryPath
                                                , message = directorySourcePathSubPathRequestError.message
                                                }
                                                    :: state.sourceDirectoryReadErrors
                                        }

                                Ok subPaths ->
                                    SingleRun
                                        { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                        , sourceDirectoriesToRead =
                                            state.sourceDirectoriesToRead
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
                                                    state.sourceFilesToRead
                                        , formattedModulesToWrite = state.formattedModulesToWrite
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFilesToRead
        |> Set.toList
        |> List.map
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            let
                                sourceBytesOrReadError : Result String Elm.Syntax.File.File
                                sourceBytesOrReadError =
                                    case sourceBytesOrError of
                                        Err sourceFileReadError ->
                                            Err sourceFileReadError.message

                                        Ok sourceBytes ->
                                            sourceBytes |> bytesToElmSyntaxModule
                            in
                            case sourceBytesOrReadError of
                                Err readError ->
                                    SingleRun
                                        { state
                                            | sourceFileReadErrors =
                                                { path = sourceFilePath
                                                , message = readError
                                                }
                                                    :: state.sourceFileReadErrors
                                        }

                                Ok syntax ->
                                    SingleRun
                                        { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                        , sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> Set.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> Dict.insert sourceFilePath
                                                    (syntax |> elmSyntaxModuleToBytes)
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoryReadErrors
        |> List.map
            (\directoryReadError ->
                Node.standardOutWrite
                    ("failed to read the source directory "
                        ++ directoryReadError.path
                        ++ ": "
                        ++ directoryReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    , state.sourceFileReadErrors
        |> List.map
            (\fileReadError ->
                Node.standardOutWrite
                    ("failed to read the source file "
                        ++ fileReadError.path
                        ++ ": "
                        ++ fileReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


bytesToElmSyntaxModule : Bytes -> Result String Elm.Syntax.File.File
bytesToElmSyntaxModule sourceBytes =
    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
        Nothing ->
            Err "source bytes couldn't be decoded into UTF-8"

        Just source ->
            case source |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ of
                Nothing ->
                    Err "source couldn't be parsed. Check for compiler errors."

                Just syntax ->
                    Ok syntax


elmSyntaxModuleToBytes : Elm.Syntax.File.File -> Bytes
elmSyntaxModuleToBytes elmSyntaxModule =
    elmSyntaxModule
        |> ElmSyntaxPrint.module_
        |> ElmSyntaxPrint.toString
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


watchInterface : WatchState -> Node.Interface State
watchInterface state =
    [ state.elmJsonSourceDirectories
        |> List.map
            (\elmJsonSourceDirectory ->
                Node.fileChangeListen elmJsonSourceDirectory
                    |> Node.interfaceFutureMap
                        (\fileChange ->
                            case fileChange of
                                Node.FileRemoved removedSubPath ->
                                    Watching state

                                Node.FileAddedOrChanged addedOrChangedPath ->
                                    if addedOrChangedPath |> String.endsWith ".elm" then
                                        Watching
                                            { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead |> Set.insert addedOrChangedPath
                                            , formattedModulesToWrite = state.formattedModulesToWrite
                                            , sourceFileReadErrors = state.sourceFileReadErrors
                                            }

                                    else
                                        Watching state
                        )
            )
        |> Node.interfaceBatch
    , state.formattedModulesToWrite
        |> Dict.toList
        |> List.map
            (\( moduleToFormatPath, moduleToFormatContent ) ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\result ->
                            Watching
                                { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                , sourceFilesToRead = state.sourceFilesToRead
                                , formattedModulesToWrite =
                                    state.formattedModulesToWrite |> Dict.remove moduleToFormatPath
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFilesToRead
        |> Set.toList
        |> List.map
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            let
                                sourceBytesOrReadError : Result String Elm.Syntax.File.File
                                sourceBytesOrReadError =
                                    case sourceBytesOrError of
                                        Err sourceFileReadError ->
                                            Err sourceFileReadError.message

                                        Ok sourceBytes ->
                                            sourceBytes |> bytesToElmSyntaxModule
                            in
                            case sourceBytesOrReadError of
                                Err readError ->
                                    Watching
                                        { state
                                            | sourceFileReadErrors =
                                                state.sourceFileReadErrors
                                                    |> Dict.insert sourceFilePath readError
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead
                                                    |> Set.remove sourceFilePath
                                        }

                                Ok syntax ->
                                    Watching
                                        { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> Set.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> Dict.insert sourceFilePath
                                                    (syntax |> elmSyntaxModuleToBytes)
                                        , sourceFileReadErrors =
                                            state.sourceFileReadErrors
                                                |> Dict.remove sourceFilePath
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFileReadErrors
        |> Dict.toList
        |> List.map
            (\( fileReadErrorPath, fileReadError ) ->
                Node.standardOutWrite
                    (Ansi.Cursor.hide
                        ++ "failed to read the source file "
                        ++ fileReadErrorPath
                        ++ ": "
                        ++ fileReadError
                        ++ "\n"
                        |> Ansi.Color.fontColor Ansi.Color.yellow
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
