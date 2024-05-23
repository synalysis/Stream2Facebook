module StateMachine exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Ports exposing (sendAction)
import Html exposing (Html, div, text)

type alias State =
    { action : Action
    , description : String
    }

type Action
    = ProcessingStartedAction
    | RetrieveDocumentLanguage
    | RetrieveCurrentUrl
    | LogAction String
    | ClickAction String
    | RetrieveAction String String (Maybe String)
    | WaitAction String Int Int (Maybe String)
    | WaitForUrlAction String Int Int
    | NestedClickAction String String 
    | ClickByTextAction String String
    | FillValueAction String
    | FillChildPValueAction String -- key for the value to fill
    | FocusElementAction String
    | StoreValueAction String
    | EraseValueAction String
    | ProcessingFinishedAction
    | ConditionalSkipAction String (Maybe String) Int Int

type StateMachineModel =
    Executing ExecutingModel
    | Succeeded { results : Dict String String}
    | Failed { error : String }

type ResultsType =
    ExecutionStillRunning { results : Dict String String}
    | ExecutionSucceeded { results : Dict String String}
    | ExecutionFailed { error : String }

type alias ExecutingModel =
    { currentIndex : Int
    , states : List State
    , dictionary : Dict String String
    , results : Dict String String
    , language : String
    }

type Msg
    = ActionCompleted
    | ActionFailed String
    | RetrieveCompleted String String
    | GotoState Int
    | ProcessingCompleted

init : List State -> Dict String String -> String -> ( StateMachineModel, Cmd Msg )
init states dictionary language =
    runAction {currentIndex = 0, states = states, dictionary = dictionary, results = Dict.empty, language = language } 

update : Msg -> StateMachineModel -> ( StateMachineModel, Cmd Msg )
update msg model =
    case msg of
        ActionCompleted ->
            case model of
                Executing executingModel ->
                    if executingModel.currentIndex < List.length executingModel.states then
                        runAction executingModel
                    else
                        ( Succeeded { results = executingModel.results }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActionFailed error ->
            ( Failed { error = error}, Cmd.none )

        GotoState newState ->
            case model of
                Executing executingModel ->
                        runAction { executingModel | currentIndex = newState }

                _ ->
                    ( model, Cmd.none )

        RetrieveCompleted key value ->
            case model of
                Executing executingModel ->
                    let
                        updatedResults = Dict.insert key value executingModel.results

                        newModel =
                            { executingModel | results = updatedResults }
                    in
                    if executingModel.currentIndex < List.length executingModel.states then
                        runAction newModel
                    else
                        ( Executing newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ProcessingCompleted ->
            case model of
                Executing executingModel ->
                    ( Succeeded { results = executingModel.results }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

subscriptions : StateMachineModel -> Sub Msg
subscriptions _ =
    Ports.receiveMessageFromContentScript decodeValueToMsg

results : StateMachineModel -> ResultsType
results model =
    case model of
        Executing executingModel ->
            ExecutionStillRunning { results = executingModel.results }

        Succeeded succeededModel ->
            ExecutionSucceeded { results = succeededModel.results }

        Failed failedModel ->
            ExecutionFailed { error = failedModel.error }

runAction : ExecutingModel -> (StateMachineModel, Cmd Msg)
runAction executingModel =
    ( Executing { executingModel | currentIndex = executingModel.currentIndex + 1}, runActionCmd executingModel)


currentState : ExecutingModel -> Maybe State
currentState executingModel =
            List.head (List.drop executingModel.currentIndex executingModel.states)

runActionCmd : ExecutingModel -> Cmd Msg
runActionCmd executingModel =
    case currentState executingModel of
        Just state ->
            let
                sendActionWithType =
                    sendAction (actionTypeKey state.action)
            in
            case state.action of
                ProcessingStartedAction ->
                    sendActionWithType
                        [ ( "language", Encode.string executingModel.language ) ]

                LogAction message ->
                    sendActionWithType
                            [ ( "message", Encode.string message) ]

                ClickAction selector ->
                    sendActionWithType
                            [ ( "selector", Encode.string selector )
                            , ( "description", Encode.string state.description )
                            ]

                RetrieveAction key selector pattern ->
                    sendActionWithType
                            [ ( "key", Encode.string key )
                            , ( "selector", Encode.string selector )
                            , ( "description", Encode.string state.description )
                            , ( "pattern", maybeEncode Encode.string pattern )
                            ]

                WaitAction selector timeout retries textContent ->
                    sendActionWithType
                            [ ( "selector", Encode.string selector )
                            , ( "timeout", Encode.int timeout )
                            , ( "retries", Encode.int retries )
                            , ( "description", Encode.string state.description )
                            , ( "textContent", maybeEncode Encode.string textContent )
                            ]

                WaitForUrlAction urlPattern timeout retries ->
                    sendActionWithType
                            [ ( "urlPattern", Encode.string urlPattern )
                            , ( "timeout", Encode.int timeout )
                            , ( "retries", Encode.int retries )
                            , ( "description", Encode.string state.description )
                            ]

                NestedClickAction parentSelector childSelector ->
                    sendActionWithType
                            [ ( "parentSelector", Encode.string parentSelector )
                            , ( "childSelector", Encode.string childSelector )
                            , ( "description", Encode.string state.description )
                            ]

                ClickByTextAction tag text ->
                    sendActionWithType
                            [ ( "tag", Encode.string tag )
                            , ( "text", Encode.string text )
                            , ( "description", Encode.string state.description )
                            ]

                FillValueAction key ->
                    let
                        value =
                            Dict.get key executingModel.dictionary
                                |> Maybe.withDefault ""
                    in
                    sendActionWithType
                            [ ( "value", Encode.string value ) ]

                FillChildPValueAction key ->
                    let
                        value =
                            Dict.get key executingModel.dictionary
                                |> Maybe.withDefault ""
                    in
                    sendActionWithType
                            [ ( "value", Encode.string value ) ]

                FocusElementAction selector ->
                    sendActionWithType
                            [ ( "selector", Encode.string selector )
                            , ( "description", Encode.string state.description )
                            ]

                StoreValueAction key ->
                    let
                        value =
                            Dict.get key executingModel.results
                                |> Maybe.withDefault ""
                    in
                    sendActionWithType
                            [ ( "key", Encode.string key )
                            , ( "value", Encode.string value )
                            ]

                EraseValueAction key ->
                    sendActionWithType
                            [ ( "key", Encode.string key ) ]

                ProcessingFinishedAction ->
                    sendActionWithType []

                ConditionalSkipAction selector maybeText stateIfFound stateIfNotFound ->
                    sendActionWithType
                        [ ( "selector", Encode.string selector )
                        , ( "text", maybeEncode Encode.string maybeText )
                        , ( "stateIfFound", Encode.int stateIfFound )
                        , ( "stateIfNotFound", Encode.int stateIfNotFound )
                        ]

                RetrieveDocumentLanguage ->
                    sendActionWithType []

                RetrieveCurrentUrl ->
                    sendActionWithType []

        Nothing ->
            Cmd.none

actionTypeKey : Action -> String
actionTypeKey actionType =
    case actionType of
                ProcessingStartedAction ->
                    "processingStarted"

                RetrieveDocumentLanguage ->
                    "retrieveDocumentLanguage"

                RetrieveCurrentUrl ->
                    "retrieveCurrentUrl"

                LogAction _ ->
                    "log"

                ClickAction _ ->
                    "click"

                RetrieveAction _ _ _ ->
                    "retrieve"

                WaitAction _ _ _ _ ->
                    "wait"

                WaitForUrlAction _ _ _ ->
                    "waitForUrl"

                NestedClickAction _ _ ->
                    "nestedClick"

                ClickByTextAction _ _ ->
                    "clickByText"

                FillValueAction _ ->
                    "fillValue"

                FillChildPValueAction _ ->
                    "fillChildPValue"

                FocusElementAction _ ->
                    "focusElement"

                StoreValueAction _ ->
                    "storeValue"

                EraseValueAction _ ->
                    "eraseValue"

                ProcessingFinishedAction ->
                    "processingFinished"

                ConditionalSkipAction _ _ _ _ ->
                    "conditionalSkip"

decodeMessage : Decode.Decoder Msg
decodeMessage =
    Decode.oneOf
        [ Decode.field "type" Decode.string
            |> Decode.andThen (\type_ ->
                case type_ of
                    "ACTION_COMPLETED" ->
                        Decode.succeed ActionCompleted

                    "ACTION_FAILED" ->
                        Decode.field "description" Decode.string |> Decode.map ActionFailed

                    "GOTO_STATE" ->
                        Decode.field "state" Decode.int |> Decode.map GotoState

                    "RETRIEVE_COMPLETED" ->
                            Decode.map2 RetrieveCompleted
                                (Decode.field "key" Decode.string)
                                (Decode.field "value" Decode.string)

                    "PROCESSING_COMPLETED" ->
                        Decode.succeed ProcessingCompleted

                    _ ->
                        Decode.fail "Unknown action type"
               )
        ]

decodeValueToMsg : Value -> Msg
decodeValueToMsg value =
    case Decode.decodeValue decodeMessage value of
        Ok msg -> msg
        Err _ -> ActionFailed "Decoding failed"

maybeEncode : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeEncode encoder maybeValue =
    case maybeValue of
        Just value -> encoder value
        Nothing -> Encode.null

view : StateMachineModel -> Html Msg
view model =
    case model of
        Executing executingModel ->
            case currentState executingModel of
                Just state ->
                    div []
                        [ div []
                            [ div []
                            [ Html.text (state.description) ]
                            ]
                        ]
                Nothing ->
                    div [] []



        _ ->
            div [][]