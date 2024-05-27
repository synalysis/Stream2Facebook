module Main exposing (..)

import Browser
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Html exposing (Html, b, button, div, form, input, label, text, textarea)
import Html.Attributes exposing (placeholder, style, name, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Dict exposing (Dict)
import StateMachine exposing (Action(..), State, StateMachineModel, init, subscriptions, update)
import Ports exposing (sendAction)
import Regex
import Html.Attributes exposing (disabled)

-- MODEL

type alias Flags =
    { url : String
    , title : String
    , streamKey : String
    , serverUrl : String
    , liveStreamingPageUrl : String
    }

type Model
    = RetrievingLanguage { maybeStreamKey : Maybe String, maybeServerUrl: Maybe String, maybeLiveStreamingPageUrl : Maybe String }
    | NonimplementedLanguage String
    | Form { title : String, description : String, liveStreamingPageUrl : String, language : Language }
    | Executing StateMachineModel Language
    | ResultsWaitingForLanguage (Dict String String)
    | Results (Dict String String) Language
    | Error String

type Language
    = English
    | German

languageFromString : String -> Maybe Language
languageFromString lang =
    case lang of
        "en" ->
            Just English

        "de" ->
            Just German

        _ ->
            Nothing

languageName : Language -> String
languageName language =
    case language of
        English ->
            "English"

        German ->
            "German"

init : Flags -> ( Model, Cmd Msg )
init flags =
    if (flags.streamKey /= "") && (flags.serverUrl /= "") && (flags.liveStreamingPageUrl /= "") then
        ( ResultsWaitingForLanguage
            (Dict.fromList
                [ (streamKeyVarKey, flags.streamKey)
                , (serverUrlVarKey, flags.serverUrl)
                , (liveStreamingPageUrlVarKey, flags.liveStreamingPageUrl)
                ]
            )
        , sendAction (StateMachine.actionTypeKey RetrieveDocumentLanguage) [ ]
        )
    else
        ( RetrievingLanguage
            { maybeStreamKey = Nothing
            , maybeServerUrl = Nothing
            , maybeLiveStreamingPageUrl = Nothing
            }
        , sendAction (StateMachine.actionTypeKey RetrieveCurrentUrl) [ ]
        )

-- UPDATE

type Msg
    = StateMachineMsg StateMachine.Msg
    | RetrievedLanguage String
    | RetrievedUrl String
    | ActionCompleted
    | ActionFailed String
    | SubmitForm
    | UpdateTitle String
    | UpdateDescription String
    | ResetResults

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StateMachineMsg smMsg ->
            case model of
                Executing stateMachine lang ->
                    let
                        ( newStateMachine, smCmd ) =
                            StateMachine.update smMsg stateMachine
                    in
                    case StateMachine.results newStateMachine of
                        StateMachine.ExecutionStillRunning _ ->
                            ( Executing newStateMachine lang, Cmd.map StateMachineMsg smCmd)

                        StateMachine.ExecutionSucceeded { results  } ->
                            ( Results results lang, Cmd.none )

                        StateMachine.ExecutionFailed { error  } ->
                            ( Error error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RetrievedLanguage lang ->
            let
                maybeLanguage =
                    languageFromString lang
            in
            case (model, maybeLanguage) of
                (ResultsWaitingForLanguage results, Just language ) ->
                    (Results results language, Cmd.none)

                (Error _, _) ->
                    (model, Cmd.none)

                (_, Nothing) ->
                    (NonimplementedLanguage lang, Cmd.none)

                (RetrievingLanguage { maybeStreamKey, maybeServerUrl, maybeLiveStreamingPageUrl }, Just language) ->
                    case (maybeStreamKey, maybeServerUrl, maybeLiveStreamingPageUrl) of
                        (Just streamKey, Just serverUrl, Just liveStreamingPageUrl ) ->
                            ( Results (Dict.fromList [ (streamKeyVarKey, streamKey), (serverUrlVarKey, serverUrl), (liveStreamingPageUrlVarKey, liveStreamingPageUrl) ]) language, Cmd.none)

                        (_, _, Just liveStreamingPageUrl ) ->
                            ( Form { title = "", description = "", language = language, liveStreamingPageUrl = liveStreamingPageUrl } , Cmd.none)

                        (_, _, _) ->
                            ( Form { title = "", description = "", language = language, liveStreamingPageUrl = "" } , Cmd.none)

                (_, _) ->
                    (Error "Invalid state", Cmd.none)

        RetrievedUrl currentUrl ->
                    if not (matchesPattern "facebook\\.com\\/groups\\/" currentUrl) then
                        (Error "This plugin works only for Facebook groups", Cmd.none)
                    else
                        modelWithCurrentUrl model currentUrl

        ActionCompleted ->
            (model, Cmd.none)

        ActionFailed error ->
            (Error error, Cmd.none)

        SubmitForm ->
            case model of
                Form { title, description, language, liveStreamingPageUrl} ->
                  let
                    ( newStateMachine, newCmd ) =
                        StateMachine.init
                            (translatedStates initialStates language)
                            (Dict.fromList [ ("title", title), ("description", description) ])
                            (languageName language) 
                            (Dict.fromList [ (liveStreamingPageUrlVarKey, liveStreamingPageUrl) ])
                 in
                 ( Executing newStateMachine language
                 , Cmd.batch [ Cmd.map StateMachineMsg newCmd ]
                 )
                _ ->
                    ( model, Cmd.none )

        UpdateTitle newTitle ->
            case model of
                Form { description, language, liveStreamingPageUrl } ->
                    ( Form { title = newTitle, description = description, language = language, liveStreamingPageUrl = liveStreamingPageUrl}
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateDescription newDescription ->
            case model of
                Form { title, language, liveStreamingPageUrl } ->
                    ( Form { title = title, description = newDescription, language = language, liveStreamingPageUrl = liveStreamingPageUrl }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ResetResults ->
            case model of
                Results _ _  ->
                    ( RetrievingLanguage
                                { maybeStreamKey = Nothing
                                , maybeServerUrl = Nothing
                                , maybeLiveStreamingPageUrl = Nothing
                                }
                    , Cmd.batch
                        [ sendAction (StateMachine.actionTypeKey (EraseValueAction streamKeyVarKey)) [ ( "key", Encode.string streamKeyVarKey) ]
                        , sendAction (StateMachine.actionTypeKey (EraseValueAction serverUrlVarKey)) [ ( "key", Encode.string serverUrlVarKey) ]
                        , sendAction (StateMachine.actionTypeKey (EraseValueAction liveStreamingPageUrlVarKey)) [ ( "key", Encode.string liveStreamingPageUrlVarKey) ]
                        , sendAction (StateMachine.actionTypeKey RetrieveCurrentUrl) [ ]
                        ]
                     )

                _ ->
                    ( model, Cmd.none )

modelWithCurrentUrl : Model -> String -> (Model, Cmd Msg)
modelWithCurrentUrl model currentUrl =
    let
        retrieveLanguageCmd =
            sendAction (StateMachine.actionTypeKey RetrieveDocumentLanguage) [ ]
    in
    case model of
        Form { title, description, language } ->
            ( Form { title = title, description = description, language = language, liveStreamingPageUrl = currentUrl }, retrieveLanguageCmd )

        RetrievingLanguage { maybeStreamKey, maybeServerUrl } ->
            (RetrievingLanguage { maybeStreamKey = maybeStreamKey, maybeServerUrl = maybeServerUrl, maybeLiveStreamingPageUrl = Just currentUrl }, retrieveLanguageCmd)

        _ ->
            (model, retrieveLanguageCmd)


matchesPattern : String -> String -> Bool
matchesPattern pattern str =
    case Regex.fromString pattern of
        Just regex ->
            Regex.contains regex str

        Nothing ->
            False

initialStates : List State
initialStates =
    [ { action = ProcessingStartedAction, description = "Processing started" }
    , { action = EraseValueAction streamKeyVarKey, description = "Erase stream key" }
    , { action = EraseValueAction serverUrlVarKey, description = "Erase server URL" }
    , { action = EraseValueAction liveStreamingPageUrlVarKey, description = "Erase live streaming page URL" }
    , { action = LogAction "Checking if the current URL is a Facebook group", description = "Logging" }
    , { action = WaitForUrlAction "facebook.com/groups" 0 1, description = "Check for Facebook group" }
    , { action = ClickAction "div.xmjcpbm.x2g32xy[role='button'][tabindex='0']", description = "Click on post" }
    , { action = WaitAction "form[method='POST']:not([class]):not([action])" 1000 5 Nothing, description = "Wait for post form" }
    , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "div[aria-label='More']", description = "Click on 'Mehr'" }
    , { action = WaitAction "span" 1000 5 (Just "^Live video$"), description = "Wait for 'Live video'" }
    , { action = ClickByTextAction "span" "^Live video$", description = "Click on 'Live video'" }
    , { action = WaitForUrlAction "/live/producer" 1000 30, description = "Wait for URL change to /live/producer" }
    , { action = WaitAction "div[aria-label='Go live'][role='button']" 1000 5 Nothing, description = "Wait for 'Go live'" }
    , { action = ClickAction "div[aria-label='Go live'][role='button']", description = "Click on 'Go live'" }
    , { action = WaitAction "div[aria-label='Streaming software']" 1000 10 Nothing, description = "Wait for 'Streaming software'" }
    , { action = ClickAction "div[aria-label='Streaming software']", description = "Click on 'Streaming software'" }
    , { action = RetrieveAction streamKeyVarKey "input[aria-label='Stream key']" Nothing, description = "Retrieve stream key" }
    , { action = ClickByTextAction "span" "Advanced Settings", description = "Click 'Advanced Settings" }
    , { action = RetrieveAction serverUrlVarKey "input[type='text']" (Just "rtmps"), description = "Retrieve server URL" }
    , { action = ConditionalSkipAction "span" (Just "What's your live video about?") 20 33, description = "Skip opening dialog if values can be entered directly" }
    -- "de" variant - extra dialog
    , { action = ClickByTextAction "span" "What's your live video about?", description = "Click on 'What's your live video about?'" }
    , { action = WaitAction "span" 1000 10 (Just "Title \\(required\\)"), description = "Wait for 'Title (required)" }
    , { action = ClickByTextAction "span" "Title \\(required\\)", description = "Click on 'Title (required)'" }
    , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "input[type='text']", description = "Click 'Titel' within dialog" }
    , { action = FillValueAction "title", description = "Fill 'title'" }
    , { action = FocusElementAction "div[aria-label='Description (Required)'][role='textbox']", description = "Focus on 'Beschreibung (erforderlich)" }
    , { action = FillChildPValueAction "description", description = "Fill 'description'" }
    , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "input[type='text']", description = "Click 'Titel' within dialog" }
    , { action = ClickAction "div[aria-label='Save'][role='button']", description = "Click 'Save'" }
    , { action = StoreValueAction streamKeyVarKey, description = "Store stream key" }
    , { action = StoreValueAction serverUrlVarKey, description = "Store server URL" }
    , { action = StoreValueAction liveStreamingPageUrlVarKey, description = "Store live streaming page URL" }
    , { action = ProcessingFinishedAction, description = "Finished" }
    -- "en" variant
    , { action = ClickByTextAction "span" "Title \\(optional\\)", description = "Click on 'Title (optional)'" }
    , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "input[type='text']", description = "Click 'Title' within dialog" }
    , { action = FillValueAction "title", description = "Fill 'title'" }
    , { action = FocusElementAction "div[aria-label='Description'][role='textbox']", description = "Focus on 'Description" }
    , { action = FillChildPValueAction "description", description = "Fill 'description'" }
    , { action = StoreValueAction streamKeyVarKey, description = "Store stream key" }
    , { action = StoreValueAction serverUrlVarKey, description = "Store server URL" }
    , { action = StoreValueAction liveStreamingPageUrlVarKey, description = "Store live streaming page URL" }
    , { action = ProcessingFinishedAction, description = "Finished" }
    ]

type alias TranslationDict = Dict String String

translationDictEn : TranslationDict
translationDictEn =
    [ ] |> Dict.fromList

translationDictDe : TranslationDict
translationDictDe =
    [ ( "div[aria-label='More']", "div[aria-label='Mehr']" )
    , ( "Live video", "Live-Video")
    , ( "^Live video$", "^Live-Video$")
    , ( "div[aria-label='Go live'][role='button']", "div[aria-label='Live gehen'][role='button']" )
    , ( "div[aria-label='Streaming software']", "div[aria-label='Streaming-Software']" )
    , ( "input[aria-label='Stream key']", "input[aria-label='Stream-Schlüssel']" )
    , ( "What's your live video about?", "Worum geht es in deinem Live-Video?" )
    , ( "Title \\(required\\)", "Titel \\(erforderlich\\)" )
    , ( "div[aria-label='Description (Required)'][role='textbox']", "div[aria-label='Beschreibung (erforderlich)'][role='textbox']" )
    , ( "div[aria-label='Save'][role='button']", "div[aria-label='Speichern'][role='button']" )
    , ( "Advanced Settings", "Erweiterte Einstellungen")
    , ( "Submit", "Absenden")
    , ( "Enter description here...", "Beschreibung eingeben")
    , ( "Title: ", "Titel: ")
    , ( "Description: ", "Beschreibung: ")
    , ( "Reset", "Zurücksetzen")
    , ( streamKeyVarKey, "Streaming-Schlüssel")
    , ( serverUrlVarKey, "Server URL")
    , ( liveStreamingPageUrlVarKey, "Live Streaming Page URL")
    ]
    |> Dict.fromList


translationDictForLanguage : Language -> TranslationDict
translationDictForLanguage lang =
    case lang of
        English ->
            translationDictEn

        German ->
            translationDictDe

translatedStates : List State -> Language -> List State
translatedStates states language =
    states
    |> List.map (translatedState (translationDictForLanguage language))

translatedState : TranslationDict -> State -> State
translatedState translationDict state =
    { action = translatedAction translationDict state.action
    , description = translatedText translationDict state.description
    }

translatedAction : TranslationDict -> Action -> Action
translatedAction translationDict action =
    case action of
        ClickAction selector ->
            ClickAction (translatedText translationDict selector) 

        RetrieveAction key selector maybePattern ->
            RetrieveAction key (translatedText translationDict selector) maybePattern

        ConditionalSkipAction selector maybeText stateIfFound stateIfNotFound ->
            ConditionalSkipAction (translatedText translationDict selector) (Maybe.map (translatedText translationDict) maybeText) stateIfFound stateIfNotFound

        WaitAction selector time retries maybeTextContent ->
            WaitAction (translatedText translationDict selector) time retries (Maybe.map (translatedText translationDict) maybeTextContent)

        NestedClickAction parentSelector childSelector ->
            NestedClickAction (translatedText translationDict parentSelector) (translatedText translationDict childSelector)

        ClickByTextAction tag text ->
            ClickByTextAction tag (translatedText translationDict text)

        FocusElementAction selector ->
            FocusElementAction (translatedText translationDict selector)

        _ ->
            action

translatedText : TranslationDict -> String -> String
translatedText translationDict text =
    case Dict.get text translationDict of
        Just translated ->
            translated
        _ ->
            text

streamKeyVarKey : String
streamKeyVarKey =
    "streamKey"

serverUrlVarKey : String
serverUrlVarKey =
    "serverUrl"

liveStreamingPageUrlVarKey : String
liveStreamingPageUrlVarKey =
    "liveStreamingPageUrl"

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        NonimplementedLanguage lang ->
            div [][ text ( "Plugin not implemented for language " ++ lang)]

        RetrievingLanguage _ ->
            div [][ text ( "Retrieving language")]

        Form { title, description, language } ->
            let
                submitDisabledAttribute =
                    if title == "" || description == "" then
                        [ disabled True]
                    else
                        []
            in
            
            div []
                [ form [ onSubmit SubmitForm, style "padding" "20px", style "background-color" "#f0f0f0", name "plugin-form" ]
                [ div [ style "margin-bottom" "20px" ]
                    [ label [] [ text (translatedText (translationDictForLanguage language) "Title: ") ]
                    , input [ type_ "text", value title, onInput UpdateTitle, style "width" "100%" ] []
                    ]
                , div [ style "margin-bottom" "20px" ]
                    [ label [] [ text (translatedText (translationDictForLanguage language) "Description: ") ]
                    , textarea [ value description, onInput UpdateDescription, placeholder (translatedText (translationDictForLanguage language) "Enter description here..."), style "width" "100%", style "height" "100px" ] []
                    ]
                , button (submitDisabledAttribute ++ [ onClick SubmitForm ]) [ text (translatedText (translationDictForLanguage language) "Submit") ]

                ]
            ]

        Executing stateMachine _ ->
            Html.map StateMachineMsg (StateMachine.view stateMachine) 

        ResultsWaitingForLanguage results ->
            view (Results results English)

        Results results language ->
            div [style "padding" "20px", style "background-color" "#f0f0f0"]
                [ div [style "margin-bottom" "20px" ] (Dict.toList results |> List.map (viewResult language))
                , button [ onClick ResetResults ] [ text (translatedText (translationDictForLanguage language) "Reset") ]
                ]

        Error error ->
            div [] [ text ("Error: " ++ error) ]


viewResult : Language -> (String, String) -> Html Msg
viewResult language (key, value) =
    div [ style "margin-bottom" "10px" ]
        [ text ( (translatedText (translationDictForLanguage language) key) ++ ": ")
        , b [] [ text value]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Executing stateMachine _ ->
            Sub.map StateMachineMsg (StateMachine.subscriptions stateMachine)

        _ ->
            Ports.receiveMessageFromContentScript decodeValueToMsg

decodeValueToMsg : Value -> Msg
decodeValueToMsg value =
    case Decode.decodeValue decodeMessage value of
        Ok msg -> msg
        Err _ -> ActionFailed "Decoding failed"

decodeMessage : Decode.Decoder Msg
decodeMessage =
    Decode.oneOf
        [ Decode.field "type" Decode.string
            |> Decode.andThen (\type_ ->
                case type_ of
                    "ACTION_COMPLETED" ->
                        Decode.succeed ActionCompleted

                    "DOCUMENT_LANGUAGE" ->
                            Decode.map RetrievedLanguage
                                (Decode.field "language" Decode.string)

                    "CURRENT_URL" ->
                            Decode.map RetrievedUrl
                                (Decode.field "url" Decode.string)

                    _ ->
                        Decode.fail "Unknown action type"
               )
        ]

-- MAIN

main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> init flags
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
