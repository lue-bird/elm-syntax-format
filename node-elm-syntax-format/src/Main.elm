port module Main exposing (main)

import Ansi.Color
import Ansi.Cursor
import Ansi.Font
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import FastDict
import FastSet
import Json.Decode
import Json.Encode
import Node


type State
    = WaitingForLaunchArguments
    | ShowingHelp
    | WaitingForElmJson { mode : ProjectMode }
    | SingleProjectRun SingleProjectRunState
    | SingleFileStandardStreamRun SingleFileStandardStreamRunState
    | Watching WatchState
    | ElmJsonReadFailed String


type SingleFileStandardStreamRunState
    = AssemblingModuleSourceFromStandardIn String
    | ModuleSourceReceived String


type alias SingleProjectRunState =
    { sourceDirectoriesToRead : FastSet.Set String
    , sourceFilesToRead : FastSet.Set String
    , formattedModulesToWrite : FastDict.Dict String Bytes
    , sourceDirectoryReadErrors : List { path : String, message : String }
    , sourceFileReadErrors : List { path : String, message : String }
    }


type alias WatchState =
    { elmJsonSourceDirectories : List String
    , sourceFilesToRead : FastSet.Set String
    , formattedModulesToWrite : FastDict.Dict String Bytes
    , sourceFileReadErrors : FastDict.Dict String String
    }


type ProjectMode
    = ProjectModeSingleRun
    | ProjectModeWatch


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
                        case
                            launchArguments
                                |> List.drop 2
                                |> List.filter
                                    (\arg ->
                                        (arg == "--yes")
                                            || (arg == "--elm-version")
                                            || (arg == "0.19.0")
                                            || (arg == "0.19.1")
                                            || (arg == "0.19")
                                            || (arg == "--elm-version=0.19")
                                            || (arg == "--elm-version=0.19.0")
                                            || (arg == "--elm-version=0.19.1")
                                    )
                        of
                            [] ->
                                WaitingForElmJson { mode = ProjectModeSingleRun }

                            [ "watch" ] ->
                                WaitingForElmJson { mode = ProjectModeWatch }

                            [ "help" ] ->
                                ShowingHelp

                            [ "--stdin" ] ->
                                ShowingHelp

                            _ ->
                                SingleFileStandardStreamRun (AssemblingModuleSourceFromStandardIn "")
                    )

        ShowingHelp ->
            nodeShowHelpText

        SingleFileStandardStreamRun singleFileStandardStreamRun ->
            interfaceSingleFileStandardStreamRun singleFileStandardStreamRun

        WaitingForElmJson waitingForElmJson ->
            nodeElmJsonRequest
                |> Node.interfaceFutureMap
                    (\elmJsonBytesOrError ->
                        case elmJsonBytesOrError of
                            Err elmJsonReadError ->
                                ElmJsonReadFailed elmJsonReadError

                            Ok elmJson ->
                                let
                                    computedElmJsonSourceDirectories : List String
                                    computedElmJsonSourceDirectories =
                                        elmJson |> elmJsonSourceDirectories
                                in
                                case waitingForElmJson.mode of
                                    ProjectModeSingleRun ->
                                        SingleProjectRun
                                            { sourceDirectoriesToRead =
                                                computedElmJsonSourceDirectories |> FastSet.fromList
                                            , sourceFilesToRead = FastSet.empty
                                            , formattedModulesToWrite = FastDict.empty
                                            , sourceFileReadErrors = []
                                            , sourceDirectoryReadErrors = []
                                            }

                                    ProjectModeWatch ->
                                        Watching
                                            { elmJsonSourceDirectories = computedElmJsonSourceDirectories
                                            , sourceFilesToRead = FastSet.empty
                                            , formattedModulesToWrite = FastDict.empty
                                            , sourceFileReadErrors = FastDict.empty
                                            }
                    )

        SingleProjectRun singleRunState ->
            singleRunInterface singleRunState

        Watching watchState ->
            watchInterface watchState

        ElmJsonReadFailed elmJsonDecodeError ->
            errorInterface elmJsonDecodeError


errorInterface : String -> Node.Interface future_
errorInterface message =
    [ Node.standardErrWrite (message ++ "\n")
    , Node.exit 1
    ]
        |> Node.interfaceBatch


nodeShowHelpText : Node.Interface future_
nodeShowHelpText =
    Node.standardOutWrite
        ("""Format elm 0.19 source directory and tests/ modules in the current project like elm-format (https://github.com/avh4/elm-format).

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


interfaceSingleFileStandardStreamRun : SingleFileStandardStreamRunState -> Node.Interface State
interfaceSingleFileStandardStreamRun singleFileStandardStreamRun =
    case singleFileStandardStreamRun of
        AssemblingModuleSourceFromStandardIn moduleSourceSoFar ->
            Node.standardInRawListen
                |> Node.interfaceFutureMap
                    (\nextModuleSourceChunkOrEnd ->
                        SingleFileStandardStreamRun
                            (case nextModuleSourceChunkOrEnd of
                                Node.StreamDataEndReached ->
                                    ModuleSourceReceived moduleSourceSoFar

                                Node.StreamDataReceived nextModuleSourceChunk ->
                                    AssemblingModuleSourceFromStandardIn
                                        (moduleSourceSoFar
                                            ++ nextModuleSourceChunk
                                            ++ ""
                                        )
                            )
                    )

        ModuleSourceReceived moduleSource ->
            case moduleSource |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ of
                Nothing ->
                    errorInterface
                        ("module failed to parse: "
                            ++ (moduleSource |> String.left 90)
                            ++ "..."
                            ++ (moduleSource |> String.right 90)
                        )

                Just moduleSyntax ->
                    Node.standardOutWrite
                        (moduleSyntax
                            |> ElmSyntaxPrint.module_
                            |> ElmSyntaxPrint.toString
                        )


nodeElmJsonRequest : Node.Interface (Result String Elm.Project.Project)
nodeElmJsonRequest =
    Node.fileRequest "elm.json"
        |> Node.interfaceFutureMap
            (\elmJsonBytesOrError ->
                case elmJsonBytesOrError of
                    Err fileReadError ->
                        Err
                            ("elm.json couldn't be read because "
                                ++ fileReadError.message
                            )

                    Ok elmJsonBytes ->
                        case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                            Nothing ->
                                Err "elm.json bytes could not be decoded into UTF-8 String"

                            Just elmJsonString ->
                                case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                    Err jsonDecodeError ->
                                        Err
                                            ("elm.json failed to parse due to "
                                                ++ (jsonDecodeError |> Json.Decode.errorToString)
                                            )

                                    Ok elmJson ->
                                        Ok elmJson
            )


singleRunInterface : SingleProjectRunState -> Node.Interface State
singleRunInterface state =
    [ state.formattedModulesToWrite
        |> fastDictToListAndMap
            (\moduleToFormatPath moduleToFormatContent ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\_ ->
                            SingleProjectRun
                                { formattedModulesToWrite =
                                    state.formattedModulesToWrite |> FastDict.remove moduleToFormatPath
                                , sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                , sourceFilesToRead = state.sourceFilesToRead
                                , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoriesToRead
        |> fastSetToListAndMap
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err sourceDirectoryReadError ->
                                    SingleProjectRun
                                        { state
                                            | sourceDirectoryReadErrors =
                                                { path = sourceDirectoryPath
                                                , message = sourceDirectoryReadError.message
                                                }
                                                    :: state.sourceDirectoryReadErrors
                                        }

                                Ok subPaths ->
                                    SingleProjectRun
                                        { sourceDirectoriesToRead =
                                            state.sourceDirectoriesToRead
                                                |> FastSet.remove sourceDirectoryPath
                                        , sourceFilesToRead =
                                            subPaths
                                                |> List.foldl
                                                    (\subPath soFar ->
                                                        if subPath |> String.endsWith ".elm" then
                                                            soFar |> FastSet.insert (sourceDirectoryPath ++ "/" ++ subPath)

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
    , Node.directorySubPathsRequest "tests"
        |> Node.interfaceFutureMap
            (\testSubPathsOrError ->
                case testSubPathsOrError of
                    Err _ ->
                        -- tests/ is optional. So all's fine
                        SingleProjectRun state

                    Ok subPaths ->
                        SingleProjectRun
                            { sourceFilesToRead =
                                subPaths
                                    |> List.foldl
                                        (\subPath soFar ->
                                            if subPath |> String.endsWith ".elm" then
                                                soFar |> FastSet.insert ("tests/" ++ subPath)

                                            else
                                                soFar
                                        )
                                        state.sourceFilesToRead
                            , sourceDirectoriesToRead = state.sourceDirectoriesToRead
                            , sourceFileReadErrors = state.sourceFileReadErrors
                            , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                            , formattedModulesToWrite = state.formattedModulesToWrite
                            }
            )
    , state.sourceFilesToRead
        |> fastSetToListAndMap
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
                                    SingleProjectRun
                                        { state
                                            | sourceFileReadErrors =
                                                { path = sourceFilePath
                                                , message = readError
                                                }
                                                    :: state.sourceFileReadErrors
                                        }

                                Ok syntax ->
                                    SingleProjectRun
                                        { sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> FastSet.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> FastDict.insert sourceFilePath
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
                                Node.FileRemoved _ ->
                                    Watching state

                                Node.FileAddedOrChanged addedOrChangedPath ->
                                    if addedOrChangedPath |> String.endsWith ".elm" then
                                        Watching
                                            { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead |> FastSet.insert addedOrChangedPath
                                            , formattedModulesToWrite = state.formattedModulesToWrite
                                            , sourceFileReadErrors = state.sourceFileReadErrors
                                            }

                                    else
                                        Watching state
                        )
            )
        |> Node.interfaceBatch
    , nodeTestsChangeListen
        |> Node.interfaceFutureMap
            (\fileChange ->
                case fileChange of
                    Node.FileRemoved _ ->
                        Watching state

                    Node.FileAddedOrChanged addedOrChangedPath ->
                        if addedOrChangedPath |> String.endsWith ".elm" then
                            Watching
                                { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                , sourceFilesToRead =
                                    state.sourceFilesToRead |> FastSet.insert addedOrChangedPath
                                , formattedModulesToWrite = state.formattedModulesToWrite
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }

                        else
                            Watching state
            )
    , state.formattedModulesToWrite
        |> fastDictToListAndMap
            (\moduleToFormatPath moduleToFormatContent ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\_ ->
                            Watching
                                { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                , sourceFilesToRead = state.sourceFilesToRead
                                , formattedModulesToWrite =
                                    state.formattedModulesToWrite |> FastDict.remove moduleToFormatPath
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFilesToRead
        |> fastSetToListAndMap
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
                                                    |> FastDict.insert sourceFilePath readError
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead
                                                    |> FastSet.remove sourceFilePath
                                        }

                                Ok syntax ->
                                    Watching
                                        { elmJsonSourceDirectories = state.elmJsonSourceDirectories
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> FastSet.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> FastDict.insert sourceFilePath
                                                    (syntax |> elmSyntaxModuleToBytes)
                                        , sourceFileReadErrors =
                                            state.sourceFileReadErrors
                                                |> FastDict.remove sourceFilePath
                                        }
                        )
            )
        |> Node.interfaceBatch
    , nodeHideCursor
    , state.sourceFileReadErrors
        |> fastDictToListAndMap
            (\fileReadErrorPath fileReadError ->
                Node.standardOutWrite
                    ("failed to read the source file "
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


nodeHideCursor : Node.Interface future_
nodeHideCursor =
    Node.standardOutWrite Ansi.Cursor.hide


nodeTestsChangeListen : Node.Interface Node.FileChange
nodeTestsChangeListen =
    Node.fileChangeListen "tests"


elmJsonSourceDirectories : Elm.Project.Project -> List String
elmJsonSourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs

        Elm.Project.Package _ ->
            packageSourceDirectories


packageSourceDirectories : List String
packageSourceDirectories =
    [ "src" ]


fastDictToListAndMap : (a -> b -> c) -> FastDict.Dict a b -> List c
fastDictToListAndMap keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value :: soFar
            )
            []


fastSetToListAndMap : (a -> b) -> FastSet.Set a -> List b
fastSetToListAndMap keyToElement fastDict =
    fastDict
        |> FastSet.foldr
            (\key soFar ->
                keyToElement key :: soFar
            )
            []


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
