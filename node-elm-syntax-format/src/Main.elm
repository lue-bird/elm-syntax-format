port module Main exposing (main)

{-| Format all elm modules in the source directories your project
-}

import Ansi.Color
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


type State
    = WaitingForElmJson
    | Running RunningState
    | ElmJsonReadFailed String
    | SourceDirectoryReadFailed { path : String, message : String }
    | SourceFileReadFailed { path : String, message : String }


type alias RunningState =
    { elmJson : Elm.Project.Project
    , sourceDirectoriesToRead : List String
    , sourceFilesToRead : List String
    , modulesToFormat :
        List
            { path : String
            , syntax : Elm.Syntax.File.File
            }
    }


initialState : State
initialState =
    WaitingForElmJson


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForElmJson ->
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
                                                Running
                                                    { elmJson = elmJson
                                                    , sourceDirectoriesToRead = elmJson |> elmJsonSourceDirectories
                                                    , sourceFilesToRead = []
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
    [ case ( running.sourceDirectoriesToRead, running.sourceFilesToRead ) of
        ( _ :: _, _ ) ->
            Node.interfaceNone

        ( _, _ :: _ ) ->
            Node.interfaceNone

        ( [], [] ) ->
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
    , running.sourceDirectoriesToRead
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
                                        { elmJson = running.elmJson
                                        , sourceDirectoriesToRead =
                                            running.sourceDirectoriesToRead
                                                |> List.filterMap
                                                    (\sourceDirectoryToRead ->
                                                        if sourceDirectoryToRead == sourceDirectoryPath then
                                                            Nothing

                                                        else
                                                            Just sourceDirectoryToRead
                                                    )
                                        , sourceFilesToRead =
                                            (subPaths
                                                |> List.filterMap
                                                    (\subPath ->
                                                        if subPath |> String.endsWith ".elm" then
                                                            Just (sourceDirectoryPath ++ "/" ++ subPath)

                                                        else
                                                            Nothing
                                                    )
                                            )
                                                ++ running.sourceFilesToRead
                                        , modulesToFormat = running.modulesToFormat
                                        }
                        )
            )
        |> Node.interfaceBatch
    , running.sourceFilesToRead
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
                                                        { elmJson = running.elmJson
                                                        , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                                                        , sourceFilesToRead =
                                                            running.sourceFilesToRead
                                                                |> List.filterMap
                                                                    (\sourceFileToRead ->
                                                                        if sourceFileToRead == sourceFilePath then
                                                                            Nothing

                                                                        else
                                                                            Just sourceFileToRead
                                                                    )
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
