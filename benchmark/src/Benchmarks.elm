module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Bitwise
import Elm.Parser
import Elm.Syntax.File
import Elm.Syntax.Range
import ElmSyntaxParserLenient
import ElmSyntaxPrintDefunctionalized
import ElmSyntaxPrintDefunctionalizedAttemptFaster
import Print
import Random


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "elm-syntax-format"
        (case sample1Parsed of
            Nothing ->
                [ Benchmark.describe "sample didn't parse" [] ]

            Just sample ->
                [ Benchmark.Alternative.rank "printing"
                    (\f -> f sample)
                    [ ( "current package implementation"
                      , moduleToStringCurrentPackage
                      )
                    , ( "attempt to make it faster"
                      , moduleToStringAttemptFaster
                      )
                    ]

                {- },, Benchmark.Alternative.rank "elm-syntax Location compare"
                   Benchmark.Alternative.rank "parsing"
                       (\f -> f sampleModuleSource)
                       [ --( "attempt at faster"
                         --, moduleToStringFasterIndent
                         --),
                         ( "current elm-syntax implementation"
                         , parseModuleElmSyntax
                         )
                       , ( "current package implementation"
                         , parseModuleCurrentPackage
                         )
                      ]
                -}
                ]
        )


locationCompareFast : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Basics.Order
locationCompareFast left right =
    if left.row - right.row < 0 then
        LT

    else if left.row - right.row > 0 then
        GT

    else
        Basics.compare left.column right.column


exampleLocations0 : List Elm.Syntax.Range.Location
exampleLocations0 =
    (Random.step (Random.list 100 locationRandomGenerator)
        (Random.initialSeed 0)
        |> Tuple.first
    )
        ++ List.repeat 10 { row = 1, column = 1 }


exampleLocations1 : List Elm.Syntax.Range.Location
exampleLocations1 =
    (Random.step (Random.list 100 locationRandomGenerator)
        (Random.initialSeed 1)
        |> Tuple.first
    )
        ++ List.repeat 10 { row = 1, column = 1 }


locationRandomGenerator : Random.Generator Elm.Syntax.Range.Location
locationRandomGenerator =
    Random.map2 (\row column -> { row = row, column = column })
        (Random.int 1 30)
        (Random.int 1 30)


moduleToStringCurrentPackage : Elm.Syntax.File.File -> String
moduleToStringCurrentPackage syntaxModule =
    syntaxModule
        |> ElmSyntaxPrintDefunctionalized.module_
        |> Print.toString


moduleToStringAttemptFaster : Elm.Syntax.File.File -> String
moduleToStringAttemptFaster syntaxModule =
    syntaxModule
        |> ElmSyntaxPrintDefunctionalizedAttemptFaster.module_
        |> Print.toString


parseModuleCurrentPackage : String -> Maybe Elm.Syntax.File.File
parseModuleCurrentPackage moduleSource =
    moduleSource
        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_


parseModuleElmSyntax : String -> Maybe Elm.Syntax.File.File
parseModuleElmSyntax moduleSource =
    moduleSource
        |> Elm.Parser.parseToFile
        |> Result.toMaybe


sample0Parsed : Maybe Elm.Syntax.File.File
sample0Parsed =
    sample0ModuleSource
        |> parseModuleCurrentPackage


sample1Parsed : Maybe Elm.Syntax.File.File
sample1Parsed =
    sample1ModuleSource
        |> parseModuleCurrentPackage


sample0ModuleSource : String
sample0ModuleSource =
    -- taken from https://github.com/dwayne/elm-conduit/blob/master/src/Main.elm
    -- Thanks! Below it's license
    {-
           Copyright 2024 Dwayne Crooks
    
       Redistribution and use in source and binary forms, with or without
       modification, are permitted provided that the following conditions are met:
    
       1. Redistributions of source code must retain the above copyright notice, this
       list of conditions and the following disclaimer.
    
       2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.
    
       3. Neither the name of the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software without
       specific prior written permission.
    
       THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
       ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
       WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
       DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
       FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
       DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
       SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
       CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
       OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
       OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    -}
    """module Main exposing (main)

import Api
import Api.GetUser as GetUser
import Browser as B
import Browser.Navigation as BN
import Data.Article exposing (Article)
import Data.Config as Config
import Data.Route as Route exposing (Route)
import Data.Slug exposing (Slug)
import Data.Token exposing (Token)
import Data.User exposing (User)
import Data.Username exposing (Username)
import Data.Viewer as Viewer exposing (Viewer)
import Html as H
import Json.Decode as JD
import Lib.Either as Either
import Lib.Task as Task
import Page.Article as ArticlePage
import Page.Editor as EditorPage
import Page.Error as ErrorPage
import Page.Home as HomePage
import Page.Login as LoginPage
import Page.NotAuthorized as NotAuthorizedPage
import Page.NotFound as NotFoundPage
import Page.Profile as ProfilePage
import Page.Register as RegisterPage
import Page.Settings as SettingsPage
import Port.Outgoing
import Task
import Time
import Url exposing (Url)


main : Program Flags Model Msg
main =
    B.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- FLAGS


type alias Flags =
    JD.Value



-- MODEL


type Model
    = LoadingUser LoadingUserModel
    | Success SuccessModel
    | Failure Error


type alias LoadingUserModel =
    { apiUrl : Url
    , url : Url
    , key : BN.Key
    , zone : Time.Zone
    }


type alias SuccessModel =
    { apiUrl : Url
    , url : Url
    , key : BN.Key
    , zone : Time.Zone
    , viewer : Viewer
    , page : Page
    , reloadPage : Bool
    , maybeArticle : Maybe Article
    }


type Page
    = Home HomePage.Model
    | Login LoginPage.Model
    | Register RegisterPage.Model
    | Settings SettingsPage.Model
    | Editor EditorPage.Model
    | Article ArticlePage.Model
    | Profile ProfilePage.Model
    | NotAuthorized
    | NotFound


type Error
    = BadConfig


init : Flags -> Url -> BN.Key -> ( Model, Cmd Msg )
init flags url key =
    case JD.decodeValue Config.decoder flags of
        Ok { apiUrl, resultMaybeToken } ->
            case resultMaybeToken of
                Ok (Just token) ->
                    initLoadingUser
                        { apiUrl = apiUrl
                        , url = url
                        , key = key
                        , token = token
                        }

                Ok Nothing ->
                    initSuccess
                        { apiUrl = apiUrl
                        , url = url
                        , key = key
                        , maybeZone = Nothing
                        , viewer = Viewer.Guest
                        }

                Err (Config.BadToken error) ->
                    initSuccess
                        { apiUrl = apiUrl
                        , url = url
                        , key = key
                        , maybeZone = Nothing
                        , viewer = Viewer.Guest
                        }
                        |> Tuple.mapSecond
                            (\\cmd ->
                                Cmd.batch
                                    [ Port.Outgoing.logError ("Bad token: " ++ JD.errorToString error)
                                    , cmd
                                    ]
                            )

        Err error ->
            ( Failure BadConfig
            , Port.Outgoing.logError ("Configuration error: " ++ JD.errorToString error)
            )


initLoadingUser :
    { apiUrl : Url
    , url : Url
    , key : BN.Key
    , token : Token
    }
    -> ( Model, Cmd Msg )
initLoadingUser { apiUrl, url, key, token } =
    ( LoadingUser
        { apiUrl = apiUrl
        , url = url
        , key = key
        , zone = Time.utc
        }
    , Cmd.batch
        [ getZone
        , GetUser.getUser
            apiUrl
            { token = token
            , onResponse = GotUserResponse
            }
        ]
    )


initSuccess :
    { apiUrl : Url
    , url : Url
    , key : BN.Key
    , maybeZone : Maybe Time.Zone
    , viewer : Viewer
    }
    -> ( Model, Cmd Msg )
initSuccess { apiUrl, url, key, maybeZone, viewer } =
    let
        ( zone, zoneCmd ) =
            case maybeZone of
                Nothing ->
                    ( Time.utc, getZone )

                Just givenZone ->
                    ( givenZone, Cmd.none )

        ( page, pageCmd ) =
            getPageFromUrl apiUrl key viewer Nothing url
    in
    ( Success
        { apiUrl = apiUrl
        , url = url
        , key = key
        , zone = zone
        , viewer = viewer
        , page = page
        , reloadPage = True
        , maybeArticle = Nothing
        }
    , Cmd.batch
        [ zoneCmd
        , pageCmd
        ]
    )


getZone : Cmd Msg
getZone =
    Task.perform GotZone Time.here


getPageFromUrl : Url -> BN.Key -> Viewer -> Maybe Article -> Url -> ( Page, Cmd Msg )
getPageFromUrl apiUrl key viewer maybeArticle url =
    case Route.fromUrl url of
        Just route ->
            getPageFromRoute apiUrl key viewer maybeArticle route

        Nothing ->
            ( NotFound, Cmd.none )


getPageFromRoute : Url -> BN.Key -> Viewer -> Maybe Article -> Route -> ( Page, Cmd Msg )
getPageFromRoute apiUrl key viewer maybeArticle route =
    case route of
        Route.Home ->
            HomePage.init
                { apiUrl = apiUrl
                , viewer = viewer
                , onChange = ChangedPage << ChangedHomePage
                }
                |> Tuple.mapFirst Home

        Route.Login ->
            withGuestForPage
                (always
                    (LoginPage.init
                        { onChange = ChangedPage << ChangedLoginPage }
                        |> Tuple.mapFirst Login
                    )
                )
                key
                viewer

        Route.Register ->
            withGuestForPage
                (always
                    (RegisterPage.init
                        { onChange = ChangedPage << ChangedRegisterPage }
                        |> Tuple.mapFirst Register
                    )
                )
                key
                viewer

        Route.Settings ->
            withUserForPage
                (\\user ->
                    SettingsPage.init
                        { imageUrl = user.imageUrl
                        , username = user.username
                        , bio = user.bio
                        , email = user.email
                        , onChange = ChangedPage << ChangedSettingsPage
                        }
                        |> Tuple.mapFirst Settings
                )
                viewer

        Route.CreateArticle ->
            getEditorPage apiUrl viewer Nothing

        Route.EditArticle slug ->
            getEditorPage apiUrl viewer (Just slug)

        Route.Article slug ->
            let
                ( eitherSlugOrArticle, usedCmd ) =
                    case maybeArticle of
                        Just article ->
                            ( if article.slug == slug then
                                Either.Right article

                              else
                                Either.Left slug
                            , Task.dispatch UsedArticleCache
                            )

                        Nothing ->
                            ( Either.Left slug
                            , Cmd.none
                            )
            in
            ArticlePage.init
                { apiUrl = apiUrl
                , viewer = viewer
                , eitherSlugOrArticle = eitherSlugOrArticle
                , onChange = ChangedPage << ChangedArticlePage
                }
                |> Tuple.mapBoth
                    Article
                    (\\initCmd ->
                        Cmd.batch
                            [ usedCmd
                            , initCmd
                            ]
                    )

        Route.Profile username ->
            getProfilePage apiUrl viewer username False

        Route.Favourites username ->
            getProfilePage apiUrl viewer username True


getEditorPage : Url -> Viewer -> Maybe Slug -> ( Page, Cmd Msg )
getEditorPage apiUrl viewer maybeSlug =
    withUserForPage
        (\\{ token } ->
            EditorPage.init
                { apiUrl = apiUrl
                , token = token
                , maybeSlug = maybeSlug
                , onChange = ChangedPage << ChangedEditorPage
                }
                |> Tuple.mapFirst Editor
        )
        viewer


getProfilePage : Url -> Viewer -> Username -> Bool -> ( Page, Cmd Msg )
getProfilePage apiUrl viewer username showFavourites =
    ProfilePage.init
        { apiUrl = apiUrl
        , maybeToken = Viewer.toToken viewer
        , username = username
        , showFavourites = showFavourites
        , onChange = ChangedPage << ChangedProfilePage
        }
        |> Tuple.mapFirst Profile



-- UPDATE


type Msg
    = ClickedLink B.UrlRequest
    | ChangedUrl Url
    | GotZone Time.Zone
    | GotUserResponse (Result (Api.Error ()) User)
    | Registered User
    | LoggedIn User
    | LoggedOut
    | UpdatedUser User
    | PublishedArticle Article
    | UsedArticleCache
    | DeletedArticle
    | ChangedRoute Route
    | ChangedPage PageMsg


type PageMsg
    = ChangedHomePage HomePage.Msg
    | ChangedLoginPage LoginPage.Msg
    | ChangedRegisterPage RegisterPage.Msg
    | ChangedSettingsPage SettingsPage.Msg
    | ChangedEditorPage EditorPage.Msg
    | ChangedArticlePage ArticlePage.Msg
    | ChangedProfilePage ProfilePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                B.Internal url ->
                    pushUrl url model

                B.External url ->
                    loadUrl url model

        ChangedUrl url ->
            changeUrl url model

        GotZone zone ->
            setZone zone model

        GotUserResponse result ->
            handleUserResponse result model

        Registered user ->
            loginUser user model

        LoggedIn user ->
            loginUser user model

        LoggedOut ->
            logout model

        UpdatedUser user ->
            updateUser user model

        PublishedArticle article ->
            showArticle article model

        UsedArticleCache ->
            clearArticleCache model

        DeletedArticle ->
            handleDeletedArticle model

        ChangedRoute route ->
            changeRoute route model

        ChangedPage pageMsg ->
            updatePage pageMsg model


pushUrl : Url -> Model -> ( Model, Cmd msg )
pushUrl url =
    withSuccessModel
        (\\subModel ->
            ( subModel
            , BN.pushUrl subModel.key (Url.toString url)
            )
        )


loadUrl : String -> Model -> ( Model, Cmd msg )
loadUrl url model =
    ( model
    , BN.load url
    )


changeUrl : Url -> Model -> ( Model, Cmd Msg )
changeUrl url =
    withSuccessModel
        (\\subModel ->
            if subModel.reloadPage then
                let
                    ( page, cmd ) =
                        getPageFromUrl subModel.apiUrl subModel.key subModel.viewer subModel.maybeArticle url
                in
                ( { subModel | url = url, page = page }
                , cmd
                )

            else
                ( { subModel | url = url, reloadPage = True }
                , Cmd.none
                )
        )


setZone : Time.Zone -> Model -> ( Model, Cmd msg )
setZone zone model =
    ( withModel
        { onLoadingUser = \\subModel -> LoadingUser { subModel | zone = zone }
        , onSuccess = \\subModel -> Success { subModel | zone = zone }
        , onFailure = always model
        }
        model
    , Cmd.none
    )


handleUserResponse : Result (Api.Error ()) User -> Model -> ( Model, Cmd Msg )
handleUserResponse result =
    withLoadingUserModel
        (\\{ apiUrl, url, key, zone } ->
            case result of
                Ok user ->
                    initSuccess
                        { apiUrl = apiUrl
                        , url = url
                        , key = key
                        , maybeZone = Just zone
                        , viewer = Viewer.User user
                        }

                Err error ->
                    initSuccess
                        { apiUrl = apiUrl
                        , url = url
                        , key = key
                        , maybeZone = Just zone
                        , viewer = Viewer.Guest
                        }
                        |> Tuple.mapSecond
                            (\\cmd ->
                                Cmd.batch
                                    [ Port.Outgoing.logError ("Unable to get user: " ++ Api.errorToString error)
                                    , cmd
                                    ]
                            )
        )


loginUser : User -> Model -> ( Model, Cmd Msg )
loginUser user =
    withSuccessModel
        (\\subModel ->
            ( { subModel | viewer = Viewer.User user }
            , Cmd.batch
                [ Port.Outgoing.saveToken user.token
                , Route.redirectToHome subModel.key
                ]
            )
        )


logout : Model -> ( Model, Cmd Msg )
logout =
    withSuccessModel
        (\\subModel ->
            ( { subModel | viewer = Viewer.Guest }
            , Cmd.batch
                [ Port.Outgoing.deleteToken
                , Route.redirectToHome subModel.key
                ]
            )
        )


updateUser : User -> Model -> ( Model, Cmd Msg )
updateUser user =
    withSuccessModel
        (\\subModel ->
            ( { subModel | viewer = Viewer.User user }
            , Port.Outgoing.saveToken user.token
            )
        )


showArticle : Article -> Model -> ( Model, Cmd Msg )
showArticle article =
    withSuccessModel
        (\\subModel ->
            ( { subModel | maybeArticle = Just article }
            , Route.redirectToArticle subModel.key article.slug
            )
        )


clearArticleCache : Model -> ( Model, Cmd Msg )
clearArticleCache =
    withSuccessModel
        (\\subModel ->
            ( { subModel | maybeArticle = Nothing }
            , Cmd.none
            )
        )


handleDeletedArticle : Model -> ( Model, Cmd Msg )
handleDeletedArticle =
    withSuccessModel
        (\\subModel ->
            ( subModel
            , Route.redirectToHome subModel.key
            )
        )


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route =
    withSuccessModel
        (\\subModel ->
            ( { subModel | reloadPage = False }
            , Route.pushUrl subModel.key route
            )
        )


updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage msg =
    withSuccessModel
        (\\subModel ->
            case msg of
                ChangedHomePage pageMsg ->
                    updateHomePage pageMsg subModel

                ChangedLoginPage pageMsg ->
                    updateLoginPage pageMsg subModel

                ChangedRegisterPage pageMsg ->
                    updateRegisterPage pageMsg subModel

                ChangedSettingsPage pageMsg ->
                    updateSettingsPage pageMsg subModel

                ChangedEditorPage pageMsg ->
                    updateEditorPage pageMsg subModel

                ChangedArticlePage pageMsg ->
                    updateArticlePage pageMsg subModel

                ChangedProfilePage pageMsg ->
                    updateProfilePage pageMsg subModel
        )


updateHomePage : HomePage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateHomePage pageMsg subModel =
    case subModel.page of
        Home pageModel ->
            HomePage.update
                { apiUrl = subModel.apiUrl
                , viewer = subModel.viewer
                , onChange = ChangedPage << ChangedHomePage
                }
                pageMsg
                pageModel
                |> Tuple.mapFirst
                    (\\newPageModel ->
                        { subModel | page = Home newPageModel }
                    )

        _ ->
            ( subModel, Cmd.none )


updateLoginPage : LoginPage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateLoginPage pageMsg subModel =
    case subModel.page of
        Login pageModel ->
            LoginPage.update
                { apiUrl = subModel.apiUrl
                , onLoggedIn = LoggedIn
                , onChange = ChangedPage << ChangedLoginPage
                }
                pageMsg
                pageModel
                |> Tuple.mapFirst
                    (\\newPageModel ->
                        { subModel | page = Login newPageModel }
                    )

        _ ->
            ( subModel, Cmd.none )


updateRegisterPage : RegisterPage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateRegisterPage pageMsg subModel =
    case subModel.page of
        Register pageModel ->
            RegisterPage.update
                { apiUrl = subModel.apiUrl
                , onRegistered = Registered
                , onChange = ChangedPage << ChangedRegisterPage
                }
                pageMsg
                pageModel
                |> Tuple.mapFirst
                    (\\newPageModel ->
                        { subModel | page = Register newPageModel }
                    )

        _ ->
            ( subModel, Cmd.none )


updateSettingsPage : SettingsPage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateSettingsPage pageMsg subModel =
    case subModel.page of
        Settings pageModel ->
            withUserForUpdate
                (\\{ token } ->
                    SettingsPage.update
                        { apiUrl = subModel.apiUrl
                        , token = token
                        , onUpdatedUser = UpdatedUser
                        , onChange = ChangedPage << ChangedSettingsPage
                        }
                        pageMsg
                        pageModel
                        |> Tuple.mapFirst
                            (\\newPageModel ->
                                { subModel | page = Settings newPageModel }
                            )
                )
                subModel

        _ ->
            ( subModel, Cmd.none )


updateEditorPage : EditorPage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateEditorPage pageMsg subModel =
    case subModel.page of
        Editor pageModel ->
            withUserForUpdate
                (\\{ token } ->
                    EditorPage.update
                        { apiUrl = subModel.apiUrl
                        , token = token
                        , onPublish = PublishedArticle
                        , onChange = ChangedPage << ChangedEditorPage
                        }
                        pageMsg
                        pageModel
                        |> Tuple.mapFirst
                            (\\newPageModel ->
                                { subModel | page = Editor newPageModel }
                            )
                )
                subModel

        _ ->
            ( subModel, Cmd.none )


updateArticlePage : ArticlePage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateArticlePage pageMsg subModel =
    case subModel.page of
        Article pageModel ->
            ArticlePage.update
                { apiUrl = subModel.apiUrl
                , onDeleteArticle = DeletedArticle
                , onChange = ChangedPage << ChangedArticlePage
                }
                pageMsg
                pageModel
                |> Tuple.mapFirst
                    (\\newPageModel ->
                        { subModel | page = Article newPageModel }
                    )

        _ ->
            ( subModel, Cmd.none )


updateProfilePage : ProfilePage.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateProfilePage pageMsg subModel =
    case subModel.page of
        Profile pageModel ->
            ProfilePage.update
                { apiUrl = subModel.apiUrl
                , viewer = subModel.viewer
                , onChangeRoute = ChangedRoute
                , onChange = ChangedPage << ChangedProfilePage
                }
                pageMsg
                pageModel
                |> Tuple.mapFirst
                    (\\newPageModel ->
                        { subModel | page = Profile newPageModel }
                    )

        _ ->
            ( subModel, Cmd.none )



-- VIEW


view : Model -> B.Document Msg
view model =
    let
        { title, body } =
            withModel
                { onLoadingUser = viewLoadingUserPage
                , onSuccess = viewSuccessPage
                , onFailure = viewFailurePage
                }
                model
    in
    { title =
        if String.isEmpty title then
            "Conduit"

        else
            title ++ " - Conduit"
    , body = body
    }


viewLoadingUserPage : LoadingUserModel -> B.Document msg
viewLoadingUserPage _ =
    { title = ""
    , body =
        [ H.text ""
        ]
    }


viewSuccessPage : SuccessModel -> B.Document Msg
viewSuccessPage { zone, viewer, page } =
    case page of
        Home pageModel ->
            HomePage.view
                { zone = zone
                , viewer = viewer
                , onLogout = LoggedOut
                , onChange = ChangedPage << ChangedHomePage
                }
                pageModel

        Login pageModel ->
            LoginPage.view
                { onChange = ChangedPage << ChangedLoginPage
                }
                pageModel

        Register pageModel ->
            RegisterPage.view
                { onChange = ChangedPage << ChangedRegisterPage
                }
                pageModel

        Settings pageModel ->
            withUserForView
                (\\user ->
                    SettingsPage.view
                        { user = user
                        , onLogout = LoggedOut
                        , onChange = ChangedPage << ChangedSettingsPage
                        }
                        pageModel
                )
                viewer

        Editor pageModel ->
            withUserForView
                (\\user ->
                    EditorPage.view
                        { user = user
                        , onLogout = LoggedOut
                        , onChange = ChangedPage << ChangedEditorPage
                        }
                        pageModel
                )
                viewer

        Article pageModel ->
            ArticlePage.view
                { zone = zone
                , viewer = viewer
                , onLogout = LoggedOut
                , onChange = ChangedPage << ChangedArticlePage
                }
                pageModel

        Profile pageModel ->
            ProfilePage.view
                { zone = zone
                , viewer = viewer
                , onLogout = LoggedOut
                , onChange = ChangedPage << ChangedProfilePage
                }
                pageModel

        NotAuthorized ->
            NotAuthorizedPage.view

        NotFound ->
            NotFoundPage.view
                { viewer = viewer
                , onLogout = LoggedOut
                }


viewFailurePage : Error -> B.Document msg
viewFailurePage BadConfig =
    ErrorPage.view
        { title = "Configuration Error"
        , message = "Please check your configuration. You can view the logs to diagnose and fix the specific problem."
        }



-- HELPERS


withLoadingUserModel : (LoadingUserModel -> ( Model, Cmd msg )) -> Model -> ( Model, Cmd msg )
withLoadingUserModel onLoadingUser model =
    let
        default =
            ( model, Cmd.none )
    in
    withModel
        { onLoadingUser = onLoadingUser
        , onSuccess = always default
        , onFailure = always default
        }
        model


withSuccessModel : (SuccessModel -> ( SuccessModel, Cmd msg )) -> Model -> ( Model, Cmd msg )
withSuccessModel onSuccess model =
    let
        default =
            ( model, Cmd.none )
    in
    withModel
        { onLoadingUser = always default
        , onSuccess = Tuple.mapFirst Success << onSuccess
        , onFailure = always default
        }
        model


withModel :
    { onLoadingUser : LoadingUserModel -> a
    , onSuccess : SuccessModel -> a
    , onFailure : Error -> a
    }
    -> Model
    -> a
withModel { onLoadingUser, onSuccess, onFailure } model =
    case model of
        LoadingUser subModel ->
            onLoadingUser subModel

        Success subModel ->
            onSuccess subModel

        Failure error ->
            onFailure error


withGuestForPage : (() -> ( Page, Cmd Msg )) -> BN.Key -> Viewer -> ( Page, Cmd Msg )
withGuestForPage toPage key viewer =
    case viewer of
        Viewer.Guest ->
            toPage ()

        Viewer.User _ ->
            ( NotFound, Route.redirectToHome key )


withUserForPage : (User -> ( Page, Cmd Msg )) -> Viewer -> ( Page, Cmd Msg )
withUserForPage toPage viewer =
    case viewer of
        Viewer.Guest ->
            ( NotAuthorized, Cmd.none )

        Viewer.User user ->
            toPage user


withUserForUpdate : (User -> ( SuccessModel, Cmd Msg )) -> SuccessModel -> ( SuccessModel, Cmd Msg )
withUserForUpdate toModel subModel =
    case subModel.viewer of
        Viewer.Guest ->
            ( subModel, Cmd.none )

        Viewer.User user ->
            toModel user


withUserForView : (User -> B.Document msg) -> Viewer -> B.Document msg
withUserForView toView viewer =
    case viewer of
        Viewer.Guest ->
            NotAuthorizedPage.view

        Viewer.User user ->
            toView user
"""


sample1ModuleSource : String
sample1ModuleSource =
    -- copied from https://github.com/tarbh-engineering/journal/blob/master/src/View.elm
    -- Thanks! The project doesn't come with a license.
    """module View exposing (view)

import Calendar exposing (Date)
import CalendarDates exposing (Day)
import CustomScalars exposing (Uuid)
import Data
import DateTime
import Day
import Element exposing (Attribute, Element, alignBottom, centerX, centerY, column, el, fill, height, html, none, padding, paddingXY, paragraph, px, rgb255, row, scrollbarY, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Email exposing (Email)
import Helpers
import Helpers.UuidDict as UD
import Helpers.View exposing (cappedHeight, cappedWidth, style, when, whenAttr, whenJust)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Time exposing (Month(..))
import Time.Format.I18n.I_en_us exposing (dayShort, monthName)
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Post, Tag, View(..))
import View.Img
import View.Misc exposing (btn2, btn3, formatDateTime, formatDay, iBtn, icon, lnk, spinner)
import View.Style exposing (abel, black, blue, ebg, fadeIn, green, grey, paper, popIn, red, rotate, sand, serif, shadow, shadowAlt, shadowNone, white)


onCtrlEnter : msg -> Decoder msg
onCtrlEnter msg =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen
            (\\( key, ctrl ) ->
                if ctrl && key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail ""
            )


onEnter : msg -> Decoder msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail ""
            )


onKeydown : List (Decoder msg) -> Attribute msg
onKeydown decoders =
    -- Can be replaced by a Decode.oneOf when bug is fixed
    -- https://github.com/elm/html/issues/180
    Decode.value
        |> Decode.andThen
            (\\val ->
                let
                    matchKeydown ds =
                        case ds of
                            decoder :: tail ->
                                val
                                    |> Decode.decodeValue decoder
                                    |> Result.map Decode.succeed
                                    |> Result.withDefault (matchKeydown tail)

                            [] ->
                                Decode.fail "No match"
                in
                matchKeydown decoders
            )
        |> Html.Events.on "keydown"
        |> Element.htmlAttribute


viewCalendar : Model -> Element Msg
viewCalendar model =
    let
        ht =
            if model.landscape then
                if model.screen.width < 800 then
                    40

                else
                    60

            else if model.area < 200000 then
                35

            else if model.area < 250000 then
                40

            else
                50

        wd =
            if model.screen.width < 450 then
                fill

            else
                px ht

        shift =
            case model.weekStart of
                Time.Mon ->
                    identity

                Time.Tue ->
                    cycle 1

                Time.Wed ->
                    cycle 2

                Time.Thu ->
                    cycle 3

                Time.Fri ->
                    cycle 4

                Time.Sat ->
                    cycle 5

                Time.Sun ->
                    cycle 6

        btnSize =
            if model.area < 200000 then
                15

            else
                25
    in
    [ [ iBtn btnSize Icons.chevron_left PrevMonth
      , [ model.month |> monthName
        , model.year |> String.fromInt
        ]
            |> String.join " "
            |> text
            |> el
                [ centerX
                , spacing 10
                , Background.color white
                , padding 10
                , Font.size 25
                ]
      , iBtn btnSize Icons.chevron_right NextMonth
      ]
        |> row [ width fill, spaceEvenly, padding 5 ]
    , Element.table
        [ spacing 5 ]
        { data = CalendarDates.weeks model.weekStart model.month model.year
        , columns =
            [ { header = weekday <| dayShort Time.Mon
              , width = wd
              , view = .mon >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Tue
              , width = wd
              , view = .tue >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Wed
              , width = wd
              , view = .wed >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Thu
              , width = wd
              , view = .thu >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Fri
              , width = wd
              , view = .fri >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Sat
              , width = wd
              , view = .sat >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Sun
              , width = wd
              , view = .sun >> viewCell model ht
              }
            ]
                |> shift
        }
    ]
        |> column
            [ spacing 10
            , width fill
                |> whenAttr (model.screen.width < 450)
            , centerX
            ]


cycle : Int -> List a -> List a
cycle n xs =
    List.drop n xs ++ List.take n xs


viewCell : Model -> Int -> Day -> Element Msg
viewCell model n day =
    let
        pst =
            model.posts
                |> Day.get day.date

        col =
            if model.month == Calendar.getMonth day.date then
                black

            else
                grey

        curr =
            Just day.date == model.current
    in
    Input.button
        [ height <| px n
        , (if curr then
            white

           else
            black
          )
            |> Font.color
        , Element.mouseOver
            [ Font.color View.Style.grey
            ]
            |> whenAttr (not model.isMobile && not curr)
        , (if day.month == EQ then
            sand

           else
            white
          )
            |> Background.color
        , none
            |> el
                [ width fill
                , height fill
                , Background.color col
                , style "transform-origin" "center"
                , popIn
                ]
            |> Element.inFront
            |> whenAttr curr
        , [ [ Calendar.getDay day.date
                |> Helpers.padNum
                |> text
                |> el
                    [ Element.alignTop
                    , Element.alignLeft
                    , Font.bold
                    , Font.size (n // 2)
                    , abel
                    ]
                |> el [ width fill, height fill ]
            , icon Icons.brightness_5 20
                |> when (model.today == day.date)
                |> el [ centerX ]
                |> el [ width fill ]
            ]
                |> row [ width fill, height fill ]
          , [ icon Icons.edit 20
                |> el [ popIn ]
                |> when (pst |> unwrap False (\\p -> isJust p.body))
                |> el [ centerX ]
                |> el [ width fill ]
            , icon Icons.assignment_turned_in 20
                |> el [ popIn ]
                |> when (pst |> unwrap False (\\p -> not <| List.isEmpty p.tags))
                |> el [ centerX ]
                |> el [ width fill ]
            ]
                |> row [ width fill, height fill ]
          ]
            |> column [ height fill, width fill ]
            |> Element.inFront
        ]
        { onPress = Just <| CellSelect day.date
        , label = none
        }


weekday : String -> Element msg
weekday =
    text >> el [ Font.bold ]


view : Model -> Html Msg
view model =
    let
        frame =
            if model.landscape then
                viewFrame model

            else
                viewFrameMobile model

        wd =
            if model.area < 200000 then
                10

            else
                20
    in
    [ none
        |> el
            [ width fill
            , height <| px 10
            , Background.color black
            ]
        |> when (not model.isMobile)
    , case model.view of
        ViewHome ->
            if model.landscape then
                viewHome model

            else
                viewHomeMobile model wd

        ViewCalendar ->
            (if model.landscape then
                viewPage model

             else
                viewPageMobile model
            )
                |> frame

        ViewSettings ->
            model.auth
                |> unwrap
                    ([ [ text "First day of the week"
                            |> el [ Font.bold, Font.size 22, abel ]
                       , [ Time.Mon
                         , Time.Tue
                         , Time.Wed
                         , Time.Thu
                         , Time.Fri
                         , Time.Sat
                         , Time.Sun
                         ]
                            |> List.map
                                (\\d ->
                                    let
                                        curr =
                                            d == model.weekStart
                                    in
                                    { onPress = Just <| WeekdaySet d
                                    , label =
                                        dayShort d
                                            |> (if model.screen.width <= 360 then
                                                    String.left 1

                                                else
                                                    identity
                                               )
                                            |> text
                                    }
                                        |> Input.button
                                            [ Font.color black
                                            , Element.mouseOver [ Font.color red ]
                                                |> whenAttr (not curr)
                                            , Font.size 17
                                            , shadow
                                                |> whenAttr curr
                                            , Background.color sand
                                                |> whenAttr curr
                                            , Border.roundEach
                                                { topLeft = 0
                                                , bottomRight = 0
                                                , topRight = 15
                                                , bottomLeft = 15
                                                }
                                            , padding 10
                                            ]
                                )
                            |> row
                                [ if model.screen.width <= 360 then
                                    spaceEvenly

                                  else
                                    spacing 5
                                , width fill
                                ]
                       ]
                        |> column [ width fill, spacing 10 ]
                     , hairline
                     , viewAbout
                     , hairline
                     , btn3 False Icons.emoji_events "Sign up now" (NavigateTo Types.RouteHome)
                        |> el [ centerX ]
                     ]
                        |> column [ spacing 30, centerX, cappedWidth 450 ]
                    )
                    (\\_ ->
                        [ btn3 model.inProgress.logout Icons.power_off "Logout" Logout
                            |> el [ centerX ]
                        , viewAbout
                        ]
                            |> column [ spacing 20, centerX ]
                    )
                |> frame

        ViewTags ->
            (if model.landscape then
                viewTags model

             else
                viewTagsMobile model
            )
                |> frame
    ]
        |> column
            [ spacing 10
            , height fill
            , width fill
            , fShrink
            ]
        |> render model.isMobile


viewAbout : Element msg
viewAbout =
    [ Element.link
        [ Element.mouseOver [ Font.color red ]
        , centerX
        ]
        { url = "mailto:hello@bolster.pro"
        , label =
            [ icon Icons.email 30, text "hello@bolster.pro" ]
                |> row [ spacing 10 ]
        }
    , Element.newTabLink [ centerX, Element.mouseOver [ Font.color red ] ]
        { url = "https://tarbh.engineering/"
        , label =
            [ text "Made by ", text "TARBH" |> el [ abel, Font.size 35 ] ]
                |> paragraph []
        }
    ]
        |> column [ spacing 30, width fill, Font.size 25 ]


render : Bool -> Element msg -> Html msg
render isMobile =
    let
        disableUserSelect =
            [ "", "-ms-", "-moz-", "-webkit-" ]
                |> List.map
                    (\\prefix ->
                        style (prefix ++ "user-select") "none"
                    )

        removeTapColor =
            style "-webkit-tap-highlight-color" "transparent"
    in
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor =
                    if isMobile then
                        Nothing

                    else
                        Just red
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
                |> (if isMobile then
                        (::) Element.noHover

                    else
                        identity
                   )
        }
        ([ height fill
         , width fill
         , View.Style.baseFont
         ]
            |> (++)
                (if isMobile then
                    removeTapColor :: disableUserSelect

                 else
                    []
                )
        )


viewTagsMobile : Model -> Element Msg
viewTagsMobile model =
    let
        tags =
            model.tags
                |> UD.values
    in
    model.tag
        |> Maybe.andThen
            (\\t ->
                UD.get t model.tags
            )
        |> unwrap
            (if List.isEmpty tags then
                viewNoTags model

             else
                [ viewTagsCol model tags
                , case model.tagsView of
                    Types.TagsCreate ->
                        [ Input.text
                            [ Border.rounded 0
                            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                            , width fill
                            , paddingXY 0 10
                            , onKeydown [ onEnter TagCreateSubmit ]
                            , Html.Attributes.id "editor"
                                |> Element.htmlAttribute
                            ]
                            { onChange = TagCreateNameUpdate
                            , label = Input.labelHidden ""
                            , placeholder =
                                serif "New tag"
                                    |> Input.placeholder []
                                    |> Just
                            , text = model.tagCreateName
                            }
                        , [ lnk "Cancel" <| TagsViewSet Types.TagsView
                          , btn2 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
                          ]
                            |> row [ spacing 10, Element.alignRight ]
                        ]
                            |> column [ spacing 10, width fill ]

                    Types.TagsView ->
                        [ [ iBtn 30 Icons.tune <| TagsViewSet Types.TagsSort
                          , [ text "Sorted by"
                            , (case model.tagsSort of
                                Types.SortName ->
                                    "name"

                                Types.SortDate ->
                                    "date"

                                Types.SortUsage ->
                                    "count"
                              )
                                |> text
                            ]
                                |> column [ spacing 5, Font.size 14 ]
                          ]
                            |> row [ spacing 10 ]
                        , iBtn 30 Icons.add <| TagsViewSet Types.TagsCreate
                        ]
                            |> row [ Element.alignBottom, width fill, spaceEvenly ]

                    Types.TagsSort ->
                        [ [ viewSortSelect Types.SortName model.tagsSort
                          , viewSortSelect Types.SortDate model.tagsSort
                          , viewSortSelect Types.SortUsage model.tagsSort
                          ]
                            |> column [ spacing 10 ]
                        , lnk "Cancel" <| TagsViewSet Types.TagsView
                        ]
                            |> column [ spacing 20 ]
                ]
                    |> column
                        [ spacing 10
                        , width fill
                        , height fill
                        ]
            )
            (viewTag model)


viewTagsCol : Model -> List Tag -> Element Msg
viewTagsCol model tags =
    tags
        |> (case model.tagsSort of
                Types.SortName ->
                    List.sortBy .name

                Types.SortDate ->
                    List.sortWith
                        (\\a b ->
                            DateTime.compare a.created b.created
                        )

                Types.SortUsage ->
                    List.sortBy (.posts >> List.length)
           )
        |> (if model.tagsSortReverse then
                List.reverse

            else
                identity
           )
        |> List.map
            (\\t ->
                let
                    curr =
                        model.tag == Just t.id
                in
                Input.button
                    [ Font.size 25
                    , width fill
                    , padding 5
                    ]
                    { onPress = Just <| TagSelect t.id
                    , label =
                        [ [ text t.name
                          , text <| String.fromInt <| List.length t.posts
                          ]
                            |> row
                                [ spaceEvenly
                                , width fill
                                ]
                        , formatDateTime t.created
                            |> text
                            |> el [ Font.size 15, Font.italic ]
                        ]
                            |> column
                                [ spacing 10
                                , width fill
                                , padding 15
                                , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 25, bottomLeft = 25 }
                                , (if curr then
                                    white

                                   else
                                    black
                                  )
                                    |> Font.color
                                , (if curr then
                                    black

                                   else
                                    sand
                                  )
                                    |> Background.color
                                , shadowAlt
                                ]
                    }
            )
        |> column
            [ spacing 10
            , scrollbarY
            , style "min-height" "auto"
            , width fill
            , height fill
            ]


viewNoTags : Model -> Element Msg
viewNoTags model =
    [ Input.text
        [ Border.rounded 0
        , width fill
        , paddingXY 0 10
        , onKeydown [ onEnter TagCreateSubmit ]
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Font.size 25
        ]
        { onChange = TagCreateNameUpdate
        , label = Input.labelHidden ""
        , placeholder =
            serif "Create your first tag"
                |> Input.placeholder []
                |> Just
        , text = model.tagCreateName
        }
    , [ [ "Restaurant"
        , "Flight"
        , "Gym"
        , "etc."
        ]
            |> List.map (text >> el [ Font.italic ])
            |> column [ spacing 5, Font.size 20 ]
      , btn3 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
            |> el [ Element.alignTop ]
      ]
        |> row [ spaceEvenly, width fill ]
    ]
        |> column [ width fill, spacing 10, centerY ]


viewSortIcon : Bool -> Types.TagsSort -> Types.TagsSort -> Element Msg
viewSortIcon rev sort active =
    let
        icn =
            case sort of
                Types.SortName ->
                    Icons.sort_by_alpha

                Types.SortDate ->
                    Icons.date_range

                Types.SortUsage ->
                    Icons.insert_chart_outlined

        curr =
            sort == active
    in
    Input.button
        [ Font.color black
        , Element.mouseOver [ Font.color blue ]
        ]
        { onPress = Just <| TagsSortSet sort
        , label =
            if curr then
                icon
                    (if rev then
                        Icons.north

                     else
                        Icons.south
                    )
                    30
                    |> el
                        [ Background.color sand
                        , padding 5
                        , Border.rounded 20
                        , shadowAlt
                        ]

            else
                icon icn 30
                    |> el
                        [ spacing 10
                        , padding 5
                        ]
        }


viewSortSelect : Types.TagsSort -> Types.TagsSort -> Element Msg
viewSortSelect sort curr =
    let
        txt =
            case sort of
                Types.SortName ->
                    "Name"

                Types.SortDate ->
                    "Date created"

                Types.SortUsage ->
                    "Usage count"

        icn =
            case sort of
                Types.SortName ->
                    Icons.sort_by_alpha

                Types.SortDate ->
                    Icons.date_range

                Types.SortUsage ->
                    Icons.insert_chart_outlined

        active =
            sort == curr
    in
    Input.button
        [ (if active then
            blue

           else
            black
          )
            |> Font.color
        ]
        { onPress = Just <| TagsSortSet sort
        , label = [ icon icn 30, text txt ] |> row [ spacing 10 ]
        }


viewTag : Model -> Tag -> Element Msg
viewTag model t =
    [ if model.tagBeingEdited == Just t.id then
        [ Input.text
            [ Border.rounded 0
            , Border.color black
            , width fill
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , onKeydown [ onEnter <| TagUpdateSubmit t ]
            ]
            { label = Input.labelHidden ""
            , onChange = TagUpdate
            , placeholder = Nothing
            , text = model.tagUpdate
            }
        , iBtn 30 Icons.close <| TagUpdateSet Nothing
        , iBtn 30 Icons.send <| TagUpdateSubmit t
        ]
            |> row [ width fill, spacing 10 ]

      else
        Input.button [ width fill, padding 10 ]
            { onPress = Just <| TagUpdateSet <| Just t
            , label =
                [ text t.name
                , icon Icons.edit 20
                ]
                    |> row [ spaceEvenly, width fill ]
            }
    , if List.isEmpty t.posts then
        viewNoPosts

      else
        t.posts
            |> List.sortWith Calendar.compare
            |> (if model.postSortReverse then
                    List.reverse

                else
                    identity
               )
            |> List.map
                (\\date ->
                    Input.button
                        [ Font.size 25
                        , Border.rounded 15
                        , width fill
                        , Background.color sand
                        , shadowAlt
                        ]
                        { onPress =
                            Types.RouteDayDetail date
                                |> NavigateTo
                                |> Just
                        , label =
                            date
                                |> formatDay
                                |> text
                                |> el
                                    [ spacing 10
                                    , padding 10
                                    , width fill
                                    ]
                        }
                )
            |> column
                [ spacing 20
                , width fill
                , Element.scrollbarY
                , height fill
                , padding 10
                ]
    , [ [ iBtn 30
            (if model.postSortReverse then
                Icons.north

             else
                Icons.south
            )
            PostSortToggle
        , text "Sort"
            |> el [ Font.italic ]
        ]
            |> row [ spacing 10 ]
            |> when (not <| List.isEmpty t.posts)
      , [ btn2 False Icons.delete "Delete" <| TagDelete t
        , iBtn 30 Icons.undo TagDeselect
        ]
            |> row [ spacing 20, Element.alignRight ]
      ]
        |> row [ width fill, spaceEvenly, Element.alignBottom ]
    ]
        |> column [ height fill, width fill, spacing 10 ]


viewNoPosts : Element Msg
viewNoPosts =
    [ text "No days with this tag."
        |> el [ centerX ]
    , btn3
        False
        Icons.calendar_today
        "Go to calendar"
        (NavigateTo Types.RouteCalendar)
        |> el [ centerX ]
    ]
        |> column [ spacing 20, padding 20, centerX ]


viewTags : Model -> Element Msg
viewTags model =
    let
        tags =
            model.tags
                |> UD.values
    in
    if List.isEmpty tags then
        viewNoTags model
            |> el [ cappedWidth 500, centerX, paddingXY 0 20 ]

    else
        [ [ [ [ viewSortIcon model.tagsSortReverse Types.SortName model.tagsSort
              , viewSortIcon model.tagsSortReverse Types.SortDate model.tagsSort
              , viewSortIcon model.tagsSortReverse Types.SortUsage model.tagsSort
              ]
                |> row [ spacing 10 ]
            , (case model.tagsSort of
                Types.SortName ->
                    "name"

                Types.SortDate ->
                    "date created"

                Types.SortUsage ->
                    "count"
              )
                |> (\\x ->
                        "Sorted by " ++ x
                   )
                |> text
                |> el [ Font.italic, Font.size 16 ]
            ]
                |> row [ spaceEvenly, width fill, paddingXY 0 10 ]
          , viewTagsCol model tags
          , [ Input.text
                [ Border.rounded 0
                , width <| px 350
                , paddingXY 0 10
                , onKeydown [ onEnter TagCreateSubmit ]
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                ]
                { onChange = TagCreateNameUpdate
                , label = Input.labelHidden ""
                , placeholder =
                    serif "New tag"
                        |> Input.placeholder []
                        |> Just
                , text = model.tagCreateName
                }
            , btn3 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
                |> el [ Element.alignRight ]
            ]
                |> column [ spacing 10, paddingXY 0 20 ]
          ]
            |> column [ cappedWidth 450, centerX, Element.alignTop, height fill ]
        , none |> el [ height fill, width <| px 1, Background.color black ]
        , model.tag
            |> Maybe.andThen
                (\\t ->
                    UD.get t model.tags
                )
            |> whenJust
                (\\t ->
                    [ if model.tagBeingEdited == Just t.id then
                        [ Input.text
                            [ Border.rounded 0
                            , Border.color black
                            , width fill
                            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                            , onKeydown [ onEnter <| TagUpdateSubmit t ]
                            ]
                            { label = Input.labelHidden ""
                            , onChange = TagUpdate
                            , placeholder = Nothing
                            , text = model.tagUpdate
                            }
                        , iBtn 30 Icons.close <| TagUpdateSet Nothing
                        , iBtn 30 Icons.send <| TagUpdateSubmit t
                        ]
                            |> row [ width fill, spacing 10 ]

                      else
                        Input.button [ width fill, padding 10 ]
                            { onPress = Just <| TagUpdateSet <| Just t
                            , label =
                                [ text t.name
                                , icon Icons.edit 20
                                ]
                                    |> row [ spaceEvenly, width fill ]
                            }
                    , if List.isEmpty t.posts then
                        viewNoPosts

                      else
                        t.posts
                            |> List.sortWith Calendar.compare
                            |> (if model.postSortReverse then
                                    List.reverse

                                else
                                    identity
                               )
                            |> List.map
                                (\\date ->
                                    Input.button
                                        [ Font.size 25
                                        , Border.rounded 15
                                        , width fill
                                        , Background.color sand
                                        , shadowAlt
                                        ]
                                        { onPress =
                                            Types.RouteDay date
                                                |> NavigateTo
                                                |> Just
                                        , label =
                                            date
                                                |> formatDay
                                                |> text
                                                |> el
                                                    [ spacing 10
                                                    , padding 10
                                                    , width fill
                                                    ]
                                        }
                                )
                            |> column
                                [ spacing 20
                                , width fill
                                , Element.scrollbarY
                                , height fill
                                , padding 10
                                ]
                    , [ [ iBtn 30
                            (if model.postSortReverse then
                                Icons.north

                             else
                                Icons.south
                            )
                            PostSortToggle
                        , text "Sort"
                            |> el [ Font.italic ]
                        ]
                            |> row [ spacing 10 ]
                            |> when (not <| List.isEmpty t.posts)
                      , [ btn2 False Icons.delete "Delete" <| TagDelete t
                        ]
                            |> row [ spacing 20, Element.alignRight ]
                      ]
                        |> row [ width fill, spaceEvenly, Element.alignBottom ]
                    ]
                        |> column [ height fill, width fill, spacing 10 ]
                )
            |> el [ cappedWidth 500, height fill, centerX, paddingXY 0 20 ]
            |> el [ width fill, height fill ]
        ]
            |> row [ centerX, spacing 30, height fill, width fill ]


viewHomeMobile : Model -> Int -> Element Msg
viewHomeMobile model pd =
    let
        adj =
            if model.screen.width < 360 then
                5

            else
                0
    in
    [ [ [ View.Img.loci (65 - adj)
            |> Element.html
            |> el []
        , [ text "BOLSTER"
                |> el
                    [ Font.size (55 - adj)
                    , Font.semiBold
                    , abel
                    , centerX
                    ]
          , serif "The secure, private journal."
                |> el
                    [ Font.size (20 - adj)
                    , centerX
                    ]
          ]
            |> column []
        ]
            |> row
                [ spacing 20
                , centerX
                , Element.alignTop
                ]
      , viewInfo model.def
      ]
        |> column
            [ width fill
            , height fill
            ]
    , if isNothing model.auth then
        viewFunnel model

      else
        btn3
            False
            Icons.phonelink
            "Return to app"
            (NavigateTo Types.RouteCalendar)
            |> el [ centerX, centerY ]
            |> el [ cappedHeight 150, width fill ]
    ]
        |> column
            [ height fill
            , width fill
            , spaceEvenly
            , fShrink
            , padding pd
            ]


viewInfo : Maybe Def -> Element Msg
viewInfo def =
    [ [ viewClick def Private
      , viewClick def Control
      , viewClick def Devices
      , viewClick def OpenSource
      ]
        |> column [ spacing 10 ]
    , def
        |> unwrap
            ([ viewLabel Private
             , viewLabel Control
             , viewLabel Devices
             , viewLabel OpenSource
             ]
                |> column [ spacing 10 ]
            )
            (\\d ->
                [ Input.button
                    [ height <| px 40, paddingXY 10 0, Font.bold ]
                    { onPress = Just <| SetDef d
                    , label =
                        defTitle d
                            |> text
                            |> el [ centerY ]
                    }
                , defText d
                    |> column [ spacing 10, height fill, paddingXY 10 0 ]
                ]
                    |> column
                        [ width fill
                        , spacing 5
                        , Element.alignTop
                        , height fill
                        , Background.color sand
                        , Border.roundEach
                            { topLeft = 0
                            , bottomRight = 0
                            , topRight = 0
                            , bottomLeft =
                                if d == Types.OpenSource then
                                    0

                                else
                                    25
                            }
                        , shadow
                        ]
            )
    ]
        |> row [ width fill, Font.size 17, centerY ]


viewBuy : Model -> Email -> Element Msg
viewBuy model email =
    let
        ready =
            not model.inProgress.buy

        cursor =
            if model.inProgress.buy then
                "wait"

            else
                "pointer"
    in
    [ [ [ ("$" ++ String.fromInt (model.charge + 10))
            |> text
            |> el [ Font.strike, Font.size 23, Element.alignBottom ]
        , ("$" ++ String.fromInt model.charge ++ " per year")
            |> text
            |> el [ Font.bold ]
        ]
            |> row [ Font.size 30, spacing 10 ]
      , "Limited discount"
            |> text
            |> el [ Font.italic, centerX, Font.size 17 ]
      ]
        |> column [ spacing 10, centerX ]
    , [ Element.newTabLink
            [ Element.mouseOver [ Element.alpha 0.7 ]
            ]
            { url = "https://stripe.com"
            , label = View.Img.stripe |> Element.html
            }
      , Input.button
            [ Background.gradient
                { angle = degrees 0
                , steps =
                    [ Element.rgb255 225 95 137
                    , Element.rgb255 13 50 77
                    ]
                }
            , Element.mouseOver
                [ Background.gradient
                    { angle = degrees 0
                    , steps =
                        [ red
                        , Element.rgb255 13 50 77
                        ]
                    }
                ]
            , width <| px 150
            , height <| px 40
            , Font.center
            , Font.color white
            , Border.rounded 10
            , shadow
            , Element.mouseDown
                [ Element.moveRight 5
                , Element.moveDown 5
                , shadowNone
                ]
                |> whenAttr ready
            , style "cursor" cursor
            ]
            { onPress =
                if ready then
                    Just <| Buy email

                else
                    Nothing
            , label =
                if model.inProgress.buy then
                    icon Icons.refresh 25
                        |> el [ rotate, centerX ]

                else
                    text "Buy now"
            }
      ]
        |> row [ spacing 10, Element.alignRight ]
    , Element.link
        [ Element.alignRight
        , Font.underline
        , Font.size 17
        , Element.mouseOver
            [ Font.color blue
            ]
        ]
        { url = model.coinbase
        , label = text "Or pay with cryptocurrency"
        }
    , [ lnk "Back" FunnelCancel
      ]
        |> row [ Element.alignRight ]
    ]
        |> column
            [ spacing 20
            , cappedWidth 400
            , centerX
            , padding 20
            , fadeIn
            , style "transform-origin" "center"
            ]


viewHome : Model -> Element Msg
viewHome model =
    [ [ View.Img.dark
            |> Element.html
            |> el
                [ height <| px 250
                , width <| px 250
                , Background.color black
                , padding 30
                ]
      , [ text "BOLSTER"
            |> el
                [ Font.size 120
                , Font.semiBold
                , abel
                , paddingXY 20 0
                ]
        , el [ Background.color black, width fill, height <| px 5 ] none
        , serif "The secure, private journal."
            |> el [ Font.size 35, centerX ]
        ]
            |> column [ spacing 10 ]
      ]
        |> row
            [ style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            , centerX
            ]
    , [ [ [ viewLn model.def Private 0.125
          , viewLn model.def Control 0.25
          , viewLn model.def Devices 0.375
          , viewLn model.def OpenSource 0.5
          ]
            |> column [ spacing 10, Element.alignLeft ]
        , model.def
            |> whenJust
                (\\d ->
                    defText d
                        |> column
                            [ padding 20
                            , spacing 20
                            , Background.color sand
                            , ebg
                            , Font.size 28
                            , width fill
                            , height fill
                            , shadow
                            , Border.roundEach
                                { topLeft = 0
                                , bottomRight = 0
                                , topRight = 25
                                , bottomLeft =
                                    if d == Types.OpenSource then
                                        0

                                    else
                                        25
                                }
                            ]
                )
        ]
            |> row [ centerX, width <| px 750 ]
      , if isNothing model.auth then
            viewFunnel model

        else
            btn3
                False
                Icons.phonelink
                "Return to app"
                (NavigateTo Types.RouteCalendar)
                |> el [ centerX ]
      ]
        |> column [ spacing 50, height fill, centerX ]
    ]
        |> column
            [ spacing 30
            , centerX
            , padding 30
            , height fill
            ]


defText : Def -> List (Element msg)
defText d =
    case d of
        OpenSource ->
            [ [ text "Built for performance and security, using the leading technologies available." ]
                |> paragraph []
            , [ text "The code can be viewed "
              , Element.newTabLink
                    [ Font.underline
                    , Element.mouseOver
                        [ Font.color blue
                        ]
                    ]
                    { url = "https://github.com/tarbh-engineering/journal"
                    , label = text "here"
                    }
              , text "."
              ]
                |> paragraph []
            ]

        Private ->
            [ [ text "Everything you write is encrypted on your device before it is saved, ensuring only you can ever read it." ]
                |> paragraph []
            ]

        Control ->
            [ [ text "Export your data in a variety of formats at any time." ]
                |> paragraph []
            ]

        Devices ->
            [ [ text "For everyday use on mobile, desktop and tablet." ]
                |> paragraph []
            , [ text "Visit this website using your devices to install for "
              , Element.newTabLink
                    [ Font.underline
                    , Element.mouseOver
                        [ Font.color blue
                        ]
                    ]
                    { url = "https://mobilesyrup.com/2020/05/24/how-install-progressive-web-app-pwa-android-ios-pc-mac/"
                    , label = text "iOS and Android"
                    }
              , text "."
              ]
                |> paragraph []
            ]


viewClick : Maybe Def -> Def -> Element Msg
viewClick c def =
    let
        icn =
            defIcon def

        curr =
            c == Just def
    in
    Input.button
        [ Background.color sand
            |> whenAttr curr
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 0, bottomLeft = 25 }
        , shadow
            |> whenAttr curr
        , centerY
        , Element.paddingEach { top = 10, bottom = 10, left = 10, right = 5 }
        ]
        { onPress = Just <| SetDef def
        , label = icon icn 20
        }


viewLabel : Def -> Element Msg
viewLabel def =
    Input.button
        [ width fill
        , height <| px 40
        , Font.underline
        , style "text-decoration-style" "dashed"
        , paddingXY 10 0
        ]
        { onPress = Just <| SetDef def
        , label =
            defTitle def
                |> text
                |> el
                    [ centerY
                    ]
        }


defTitle : Def -> String
defTitle def =
    case def of
        Private ->
            "End-to-end encrypted privacy"

        Devices ->
            "For use on every device"

        OpenSource ->
            "Open source codebase"

        Control ->
            "Full control over your data"


defIcon : Def -> Icon msg
defIcon d =
    case d of
        Private ->
            Icons.lock

        Control ->
            Icons.save

        Devices ->
            Icons.devices

        OpenSource ->
            Icons.public


viewLn : Maybe Def -> Def -> Float -> Element Msg
viewLn c def fl =
    let
        icn =
            defIcon def

        curr =
            c == Just def
    in
    Input.button
        [ Background.color sand |> whenAttr curr
        , width fill
        , shadow
            |> whenAttr curr
        , Font.bold
            |> whenAttr curr
        , padding 10
        , Element.mouseOver [ Font.color blue ]
        , Element.transparent True
        , style "animation-name" "fadeIn"
        , style "animation-duration" "0.5s"
        , style "animation-fill-mode" "forwards"
        , style "animation-delay" (String.fromFloat fl ++ "s")
        , Border.roundEach
            { topLeft = 0
            , bottomRight = 0
            , topRight = 0
            , bottomLeft = 25
            }
        ]
        { onPress = Just <| SetDef def
        , label =
            [ icon icn 30
                |> el
                    [ padding 10
                    , Background.color sand
                        |> whenAttr (not curr)
                    , Border.rounded 25
                    , shadow
                        |> whenAttr (not curr)
                    ]
            , defTitle def
                |> text
                |> el
                    [ Font.size 20
                    , paddingXY 10 0
                    ]
            ]
                |> row [ spacing 5 ]
        }


viewFunnel : Model -> Element Msg
viewFunnel model =
    let
        ent =
            if model.funnel == Hello then
                EmailSubmit

            else
                FunnelCancel
    in
    case model.funnel of
        Hello ->
            [ [ Input.email
                    [ Border.rounded 0
                    , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
                    , paddingXY 0 10
                    , width fill
                    , onKeydown [ onEnter ent ]
                    , Font.size 24
                    , ebg
                    , Font.italic
                    ]
                    { onChange = LoginFormEmailUpdate
                    , label = Input.labelHidden ""
                    , placeholder =
                        serif "Your email address"
                            |> Input.placeholder []
                            |> Just
                    , text = model.loginForm.email
                    }
              , btn2 model.inProgress.login Icons.send "Submit" ent
                    |> el [ Element.alignRight ]
              ]
                |> column
                    [ spacing 10
                    , Element.alignRight
                    , cappedWidth 450
                    ]
            , btn3
                False
                Icons.sports_esports
                "Try the demo"
                (NavigateTo Types.RouteCalendar)
                |> el [ centerX ]
            ]
                |> column
                    [ width fill
                    , cappedHeight 200
                    , spaceEvenly
                    ]

        PayErr ->
            [ [ text "The payment process was not completed." ]
                |> paragraph [ Font.center ]
            , lnk "Continue" FunnelCancel
                |> el [ centerX ]
            ]
                |> column
                    [ padding 20
                    , spacing 20
                    , Background.color sand
                    , cappedWidth 450
                    , shadow
                    , Border.rounded 25
                    , Element.alignRight
                    , popIn
                    ]

        PayOk ->
            [ text "Thank you for your purchase."
                |> el [ centerX, Font.bold ]
            , [ text "Please check your email inbox to proceed." ]
                |> paragraph [ Font.center ]
            , lnk "Close" FunnelCancel
                |> el [ centerX ]
            ]
                |> column
                    [ padding 20
                    , spacing 20
                    , Background.color sand
                    , cappedWidth 450
                    , shadow
                    , Border.rounded 25
                    , Element.alignRight
                    , popIn
                    ]

        Signup ciph ->
            model.magic
                |> whenJust
                    (\\b ->
                        if b then
                            (\\keys nonce ->
                                Data.signup keys.serverKey nonce ciph
                            )
                                |> SignupSubmit
                                |> viewSignup model

                        else
                            [ text "This link is broken."
                            , btn3 False Icons.send "Continue" FunnelCancel
                                |> el [ centerX ]
                            ]
                                |> column
                                    [ centerX
                                    , spacing 20
                                    ]
                    )

        WelcomeBack email nonce ->
            viewWelcome model nonce email

        JoinUs email ->
            viewBuy model email

        SwFail ->
            [ text "This browser may not be compatible with Bolster." ]
                |> paragraph []
                |> el
                    [ cappedWidth 400
                    , Element.alignRight
                    , Background.color sand
                    , padding 20
                    , Border.roundEach
                        { topLeft = 0
                        , bottomRight = 0
                        , topRight = 25
                        , bottomLeft = 25
                        }
                    , shadow
                    ]

        GuestSignup email ->
            (\\keys nonce ->
                Data.join keys.serverKey nonce email
            )
                |> SignupSubmit
                |> viewSignup model


viewSignup : Model -> Msg -> Element Msg
viewSignup model msg =
    [ text "Welcome"
        |> el [ Font.bold ]
    , [ text "Choose a password to protect your account" ]
        |> paragraph [ Font.italic ]
    , Input.currentPassword
        [ Border.rounded 0
        , Border.width 1
        , width fill
        , Border.widthEach
            { top = 0
            , bottom = 1
            , left = 0
            , right = 0
            }
        , paddingXY 0 10
        ]
        { onChange = LoginFormPasswordUpdate
        , label = Input.labelHidden ""
        , show = False
        , placeholder =
            serif "Password"
                |> Input.placeholder []
                |> Just
        , text = model.loginForm.password
        }
    , Input.currentPassword
        [ Border.rounded 0
        , Border.width 1
        , width fill
        , Border.widthEach
            { top = 0
            , bottom = 1
            , left = 0
            , right = 0
            }
        , paddingXY 0 10
        ]
        { onChange = LoginFormPasswordConfirmUpdate
        , label = Input.labelHidden ""
        , show = False
        , placeholder =
            serif "Confirm password"
                |> Input.placeholder []
                |> Just
        , text = model.loginForm.passwordConfirm
        }
    , [ model.loginForm.err
            |> whenJust (text >> el [ Font.color red ])
      , [ lnk "Cancel" FunnelCancel
        , btn3 model.inProgress.login Icons.send "Submit" msg
        ]
            |> row [ Element.alignRight, spacing 20 ]
      ]
        |> row [ width fill ]
    ]
        |> column
            [ cappedWidth 450
            , spacing 20
            , Element.alignRight
            ]


viewWelcome : Model -> String -> Email -> Element Msg
viewWelcome model nonce email =
    [ text "Welcome back"
        |> el [ Font.bold, Font.size 28 ]
    , [ Input.currentPassword
            [ Border.rounded 0
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , paddingXY 0 10
            , width fill
            , onKeydown [ onEnter <| LoginSubmit email nonce ]
            ]
            { onChange = LoginFormPasswordUpdate
            , label = Input.labelHidden ""
            , placeholder =
                serif "Your password"
                    |> el [ centerY ]
                    |> Input.placeholder []
                    |> Just
            , text = model.loginForm.password
            , show = False
            }
      , [ lnk "Back" FunnelCancel
        , btn3 model.inProgress.login Icons.save "Submit" (LoginSubmit email nonce)
        ]
            |> row [ Element.alignRight, spacing 10 ]
      ]
        |> column
            [ width fill
            , spacing 10
            ]
    ]
        |> column
            [ cappedWidth 450
            , Element.alignRight
            , spacing 10
            , fadeIn
            , centerY
            ]
        |> el
            [ cappedHeight 200
            , width fill
            ]


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    [ [ Input.button
            [ Font.semiBold
            , Element.mouseOver [ Font.color red ]
            ]
            { onPress = Just <| NavigateTo Types.RouteHome
            , label =
                [ View.Img.loci 50
                    |> Element.html
                    |> el []
                , text "BOLSTER" |> el [ abel, Font.size 40 ]
                , serif "DEMO"
                    |> el [ Font.light, Font.size 25 ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , [ viewNavButton blue Icons.event "Write" Types.RouteCalendar (model.view == ViewCalendar)
        , viewNavButton red Icons.assignment_turned_in "Tags" Types.RouteTags (model.view == ViewTags)
        , viewNavButton green Icons.settings "Settings" Types.RouteSettings (model.view == ViewSettings)
        ]
            |> row [ spacing 40 ]
      ]
        |> row [ width fill, spaceEvenly ]
    , elem
    ]
        |> column [ spacing 10, height fill, cappedWidth 1375, centerX ]


viewNavButton : Element.Color -> Icon Msg -> String -> Types.Route -> Bool -> Element Msg
viewNavButton col icn n r curr =
    Input.button
        [ padding 10
        , Font.size 20
        , if curr then
            Font.bold

          else
            Font.light
        , Element.mouseOver [ Font.color red ]
            |> whenAttr (not curr)
        ]
        { onPress =
            if curr then
                Nothing

            else
                Just <| NavigateTo r
        , label =
            [ icon icn 30
                |> el
                    [ centerX
                    , centerY
                    , Font.color white
                        |> whenAttr curr
                    ]
                |> el
                    [ width <| px 50
                    , height <| px 50
                    , none
                        |> el
                            [ Background.color col
                            , Border.rounded 25
                            , style "transform-origin" "center"
                            , popIn
                            , width fill
                            , height fill
                            ]
                        |> Element.behindContent
                        |> whenAttr curr
                    ]
            , text n
            ]
                |> row
                    [ spacing 5
                    ]
        }


viewFrameMobile : Model -> Element Msg -> Element Msg
viewFrameMobile model elem =
    let
        pic =
            if model.area < 200000 then
                25

            else
                50
    in
    [ [ Input.button []
            { onPress = Just <| NavigateTo Types.RouteHome
            , label =
                [ View.Img.loci pic
                    |> Element.html
                    |> el []
                , text "DEMO"
                    |> el
                        [ Font.semiBold
                        , Font.size 25
                        ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , (case model.view of
            ViewCalendar ->
                "Write"

            ViewTags ->
                "Tags"

            _ ->
                "Settings"
        )
            |> text
            |> el [ Font.italic ]
      ]
        |> row
            [ width fill
            , spaceEvenly
            ]
    , elem
    , viewBottomBar model
        |> when (model.screen.height >= View.Misc.tallInt)
    ]
        |> column
            [ spaceEvenly
            , height fill
            , width fill
            , fShrink
            , padding 10
            ]


viewBottomBar : Model -> Element Msg
viewBottomBar model =
    let
        iconSize =
            if model.area < 200000 then
                30

            else if model.area < 300000 then
                40

            else
                50
    in
    [ ( Icons.settings, Types.RouteSettings, ViewSettings )
    , ( Icons.event, Types.RouteCalendar, ViewCalendar )
    , ( Icons.assignment_turned_in, Types.RouteTags, ViewTags )
    ]
        |> List.map
            (\\( n, r, v ) ->
                let
                    curr =
                        v == model.view

                    col =
                        case r of
                            Types.RouteSettings ->
                                green

                            Types.RouteCalendar ->
                                blue

                            Types.RouteTags ->
                                red

                            _ ->
                                View.Style.garish
                in
                Input.button
                    [ Font.underline |> whenAttr curr
                    , width fill
                    , (if curr then
                        white

                       else
                        black
                      )
                        |> Font.color
                    , height <| px <| iconSize + 15
                    ]
                    { onPress =
                        if curr then
                            Nothing

                        else
                            Just <| NavigateTo r
                    , label =
                        none
                            |> el
                                [ icon n iconSize
                                    |> el
                                        [ centerX
                                        , centerY
                                        ]
                                    |> Element.inFront
                                , none
                                    |> el
                                        [ width <| px <| iconSize + 15
                                        , height <| px <| iconSize + 15
                                        , Background.color col
                                        , style "transform-origin" "center"
                                        , popIn
                                        , Border.rounded 50
                                        , centerX
                                        , centerY
                                        ]
                                    |> Element.behindContent
                                    |> whenAttr curr
                                , centerX
                                , height fill
                                ]
                    }
            )
        |> row [ Element.alignBottom, width fill ]


viewPage : Model -> Element Msg
viewPage model =
    let
        wd =
            if model.area < 200000 then
                10

            else
                20

        tags =
            UD.values model.tags
    in
    [ [ viewCalendar model
            |> el
                [ Element.alignTop
                ]
      , [ model.current
            |> whenJust
                (\\d ->
                    let
                        tagIds =
                            model.posts
                                |> Day.get d
                                |> unwrap [] (.tags >> List.map .tag)
                    in
                    [ tags
                        |> (case model.tagsSort of
                                Types.SortName ->
                                    List.sortBy .name

                                Types.SortDate ->
                                    List.sortWith
                                        (\\a b ->
                                            DateTime.compare a.created b.created
                                        )

                                Types.SortUsage ->
                                    List.sortBy (.posts >> List.length)
                           )
                        |> (if model.tagsSortReverse then
                                List.reverse

                            else
                                identity
                           )
                        |> List.map (viewPostTag model tagIds d)
                        |> column
                            [ spacing 10
                            , paddingXY 5 0
                            , scrollbarY
                            , style "min-height" "auto"
                            , width fill
                            , height fill
                            ]
                    , [ [ viewSortIcon model.tagsSortReverse Types.SortName model.tagsSort
                        , viewSortIcon model.tagsSortReverse Types.SortDate model.tagsSort
                        , viewSortIcon model.tagsSortReverse Types.SortUsage model.tagsSort
                        ]
                            |> row [ spacing 10, width fill ]
                      , (case model.tagsSort of
                            Types.SortName ->
                                "name"

                            Types.SortDate ->
                                "date"

                            Types.SortUsage ->
                                "count"
                        )
                            |> (++) "Sorted by "
                            |> text
                            |> el [ spacing 5, Font.size 17 ]
                      ]
                        |> row [ width fill, paddingXY 0 10 ]
                    ]
                        |> column [ height fill, width fill ]
                )
            |> when (not <| List.isEmpty tags)
        ]
            |> row [ width fill, height fill ]
      ]
        |> column [ height fill, spacing 20 ]
    , [ model.current
            |> unwrap
                viewReady
                (viewPost model)
      , viewTodayBtn model.screen
            |> el
                [ centerY
                ]
            |> when
                (model.current
                    /= Just model.today
                    || (model.current
                            |> unwrap False (\\c -> model.month /= Calendar.getMonth c)
                       )
                )
            |> el
                [ height <| px 70
                , Element.alignRight
                ]
      ]
        |> column [ width fill, height fill ]
    ]
        |> row [ width fill, height fill, spacing wd ]


viewPageMobile : Model -> Element Msg
viewPageMobile model =
    if model.postView then
        model.current
            |> whenJust (viewPostView model)

    else
        [ viewCalendar model
            |> el
                [ Element.alignTop
                , width fill
                ]
        , model.current
            |> unwrap
                (viewTodayBtn model.screen
                    |> el [ centerX, centerY ]
                )
                (viewBarMobile model)
        ]
            |> column
                [ width fill
                , height fill
                , spaceEvenly
                , fShrink
                ]


viewPostView : Model -> Date -> Element Msg
viewPostView model d =
    let
        pst =
            model.posts
                |> Day.get d

        fs =
            25

        txt =
            pst
                |> unwrap
                    (Just "Creating new entry")
                    (always
                        (if model.postBeingEdited then
                            Just "Updating entry"

                         else
                            Nothing
                        )
                    )

        dayTxt =
            formatDay d
                |> text
                |> el [ width fill ]

        body =
            if model.postBeingEdited then
                model.postEditorBody

            else
                pst
                    |> Maybe.andThen .body
                    |> Maybe.withDefault ""
    in
    if model.tagView then
        [ [ dayTxt
          , text "|"
          , text "Tags"
                |> el [ Element.alignRight ]
                |> el [ Font.italic, width fill ]
          ]
            |> row [ spaceEvenly, Font.size 17, width fill ]
        , viewPostTags model d pst
        , [ btn2 False Icons.edit "Write" <| NavigateTo <| Types.RouteDayDetail d
          , iBtn 30 Icons.expand_more <| NavigateTo Types.RouteCalendar
          ]
            |> row [ width fill, spaceEvenly, alignBottom, width fill ]
        ]
            |> column
                [ height fill
                , width fill
                , spacing 10
                , fShrink
                ]

    else
        [ [ dayTxt
          , text "|"
                |> when (txt /= Nothing)
          , txt
                |> whenJust
                    (text
                        >> el [ Element.alignRight ]
                        >> el [ Font.italic, width fill ]
                    )
          ]
            |> row [ spaceEvenly, Font.size 17, width fill ]
        , viewPostEditor
            body
            (not model.postBeingEdited)
            fs
            10
        , if model.postBeingEdited then
            [ lnk "Cancel" PostUpdateCancel
            , btn2 model.inProgress.post
                Icons.save
                "Submit"
                PostBodySubmit
            ]
                |> row [ spacing 20, Element.alignRight ]

          else
            [ btn2 False Icons.assignment_turned_in "Tags" <| NavigateTo <| Types.RouteDayTags d
            , btn2 False Icons.edit "Edit" PostUpdateStart
            , iBtn 30 Icons.expand_more <| NavigateTo Types.RouteCalendar
            ]
                |> row [ width fill, spaceEvenly, alignBottom, width fill ]
        ]
            |> column
                [ height fill
                , width fill
                , spacing 10
                , fShrink
                ]


viewBarMobile : Model -> Date -> Element Msg
viewBarMobile model day =
    let
        pst =
            model.posts
                |> Day.get day

        body =
            pst
                |> Maybe.andThen .body
                |> Maybe.withDefault ""
    in
    [ [ viewTodayBtn model.screen
            |> el [ centerX ]
            |> when (day /= model.today || model.month /= Calendar.getMonth day)
      , formatDay day
            |> text
            |> el
                [ (if model.screen.width < 360 then
                    14

                   else
                    16
                  )
                    |> Font.size
                , Font.italic
                , Element.alignRight
                ]
      ]
        |> column [ width fill, Element.alignBottom, spacing 10 ]
        |> el [ width fill, height fill ]
    , [ Input.button [ height fill, width fill ]
            { onPress = Just <| NavigateTo <| Types.RouteDayDetail day
            , label =
                [ viewPreview body
                , icon Icons.edit 20
                    |> el [ Element.alignTop ]
                ]
                    |> row [ spacing 10, height fill, width fill ]
            }
      , Input.button [ width fill ]
            { onPress = Just <| NavigateTo <| Types.RouteDayTags day
            , label =
                [ pst
                    |> unwrap 0
                        (.tags >> List.length)
                    |> String.fromInt
                    |> text
                , icon Icons.assignment_turned_in 20
                ]
                    |> row [ spacing 10, Element.alignRight ]
            }
      ]
        |> column
            [ height fill
            , width fill
            , spacing 10
            ]
        |> el
            [ Border.widthEach { bottom = 0, top = 0, left = 1, right = 0 }
            , paddingXY 5 0
            , width fill
            , height fill
            ]
    ]
        |> row
            [ spacing 10
            , Helpers.View.cappedHeight 175
            , paddingXY 0 10
            , width fill
            ]


viewTodayBtn : Types.Screen -> Element Msg
viewTodayBtn screen =
    Input.button
        [ Element.paddingXY 15 0
        , Font.color black
        , height <| px 50
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 25, bottomLeft = 25 }
        , Background.color sand
        , shadow
        , Font.size 17
        , popIn
        , Element.mouseOver
            [ Background.color black
            , Font.color white
            ]
        ]
        { onPress = Just <| GoToToday Nothing
        , label =
            [ icon Icons.brightness_5 20
            , (if screen.width < 360 then
                "Today"

               else
                "Go to today"
              )
                |> text
            ]
                |> row [ spacing 10 ]
        }


viewReady : Element Msg
viewReady =
    Input.button
        [ width fill
        , height fill
        , Background.color paper
        , style "cursor" View.Img.pencil
        , shadow
        ]
        { onPress = Just <| ReadyStart Nothing
        , label = none
        }


viewPost : Model -> Date -> Element Msg
viewPost model d =
    let
        fs =
            30

        pst =
            model.posts
                |> Day.get d

        body =
            if model.postBeingEdited then
                model.postEditorBody

            else
                pst
                    |> Maybe.andThen .body
                    |> Maybe.withDefault ""

        topBar =
            [ formatDay d
                |> text
                |> el [ Font.size 30, abel, Font.color white, Background.color black, padding 10 ]
            , [ lnk "Cancel" PostUpdateCancel
                    |> when model.postBeingEdited
              , if model.postBeingEdited then
                    pst
                        |> unwrap
                            (btn2 model.inProgress.post
                                Icons.save
                                "Save"
                                PostBodySubmit
                                |> when (model.postEditorBody /= "")
                            )
                            (\\p ->
                                btn2 model.inProgress.post
                                    Icons.save
                                    "Save"
                                    PostBodySubmit
                                    |> when (p.body /= Just model.postEditorBody)
                            )

                else
                    btn2 False
                        Icons.edit
                        "Edit"
                        PostUpdateStart
              ]
                |> row [ spacing 20 ]
            ]
                |> row
                    [ spaceEvenly
                    , Font.size 17
                    , width fill
                    ]
    in
    [ topBar
    , viewPostEditor body (not model.postBeingEdited) fs 20
    ]
        |> column
            [ height fill
            , width fill
            , spacing 10
            , fShrink
            ]


viewPostEditor : String -> Bool -> Int -> Int -> Element Msg
viewPostEditor txt disable fontSize pd =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "font-family" "inherit"
        , Html.Attributes.style "cursor" "inherit"
        , Html.Attributes.style "line-height" (String.fromInt (fontSize + 5) ++ "px")
        , Html.Attributes.style "padding" "0px"
        , Html.Attributes.style "flex-grow" "inherit"
        , Html.Attributes.readonly disable
        , Html.Events.onInput BodyUpdate
        ]
        []
        |> Element.html
        |> el
            [ width fill
            , (if disable then
                View.Img.pencil

               else
                "text"
              )
                |> style "cursor"
            , height fill
            , Background.color paper
            , Font.size fontSize
            , padding pd
            , ebg
            , Element.Events.onClick PostUpdateStart
                |> whenAttr disable
            , onKeydown [ onCtrlEnter PostBodySubmit ]
            , shadow
            ]


viewPreview : String -> Element Msg
viewPreview txt =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "font-family" "inherit"
        , Html.Attributes.style "font-color" "inherit"
        , Html.Attributes.style "padding" "0px"
        , Html.Attributes.style "flex-grow" "inherit"
        , Html.Attributes.style "height" "inherit"
        , Html.Attributes.style "min-height" "auto"
        , Html.Attributes.readonly True
        , Html.Attributes.style "overflow" "hidden"
        ]
        []
        |> Element.html
        |> el
            [ width fill
            , height fill
            , Background.color paper
            , Font.color black
            , Font.size 14
            , padding 5
            , ebg
            , el
                [ Helpers.View.cappedHeight 35
                , width fill
                , Element.alignBottom
                , Background.gradient
                    { angle = degrees 0
                    , steps =
                        [ View.Style.paperAlpha 1
                        , View.Style.paperAlpha 0.8
                        , View.Style.paperAlpha 0.7
                        , View.Style.paperAlpha 0.5
                        , View.Style.paperAlpha 0.4
                        ]
                    }
                ]
                none
                |> Element.inFront
            ]


viewPostTags : Model -> Date -> Maybe Post -> Element Msg
viewPostTags model d pst =
    let
        xs =
            UD.values model.tags

        postTagIds =
            pst |> unwrap [] (.tags >> List.map .tag)
    in
    if List.isEmpty xs then
        [ [ text "You don't have any tags." ]
            |> paragraph []
        , btn3
            False
            Icons.assignment_turned_in
            "Go to tags"
            (NavigateTo Types.RouteTags)
            |> el [ centerX ]
        ]
            |> column [ spacing 20, padding 20, centerX, centerY ]

    else
        xs
            |> List.map (viewPostTag model postTagIds d)
            |> List.intersperse hairline
            |> column
                [ spacing 10
                , width fill
                , centerX
                , height fill
                , Element.scrollbarY
                , style "min-height" "auto"
                ]


viewPostTag : Model -> List Uuid -> Date -> Tag -> Element Msg
viewPostTag model postTagIds d t =
    let
        flip =
            List.member t.id postTagIds

        prog =
            List.member ( d, t.id ) model.inProgress.postTags
    in
    [ Input.button [ width fill ]
        { onPress = Just <| TagSelect t.id
        , label = paragraph [] [ text t.name ]
        }
    , Input.button
        [ width <| px 40
        , height <| px 40
        , Border.width 1
        , (if prog then
            sand

           else
            white
          )
            |> Background.color
        ]
        { onPress =
            if prog then
                Nothing

            else if flip then
                Just <| PostTagDetach d t.id

            else
                Just <| PostTagAttach d t.id
        , label =
            if prog then
                spinner

            else
                icon Icons.done 20
                    |> el [ centerX, centerY ]
                    |> when flip
        }
    ]
        |> row
            [ width fill
            , spacing 10
            ]


ellipsisText : Int -> String -> Element msg
ellipsisText n txt =
    Html.div
        [ Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "text-overflow" "ellipsis"
        , Html.Attributes.style "white-space" "nowrap"
        , Html.Attributes.style "height" <| String.fromInt n ++ "px"
        , Html.Attributes.style "display" "table-cell"
        , Html.Attributes.title txt
        ]
        [ Html.text txt
        ]
        |> Element.html
        |> el
            [ width fill
            , style "width" "100%"
            , style "table-layout" "fixed"
            , style "display" "table"
            ]


{-| To handle scrollbarY problems.
<https://github.com/mdgriffith/elm-ui/issues/149>
-}
fShrink : Attribute msg
fShrink =
    style "flex-shrink" "1"
        |> whenAttr False


hairline : Element msg
hairline =
    el [ height <| px 1, width fill, Background.color black ] none"""
