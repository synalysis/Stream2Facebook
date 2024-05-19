module StateMachine exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Ports
import Task exposing (succeed, perform) 

type alias State =
    { action : Action
    , nextState : Int
    }

type Action
    = LogAction String
    | ClickAction String String
    | RetrieveAction String String String (Maybe String)
    | WaitAction String Int Int String (Maybe String)
    | WaitForUrlAction String Int Int String
    | NestedClickAction String String String
    | ClickByTextAction String String String
    | FillValueAction String
    | FillChildPValueAction String -- key for the value to fill
    | FocusElementAction String String
    | ProcessingFinishedAction

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
    }

type Msg
    = ActionCompleted
    | ActionFailed String
    | RetrieveCompleted String String
    | ProcessingCompleted

init : List State -> Dict String String -> ( StateMachineModel, Cmd Msg )
init states dictionary =
    runAction {currentIndex = 0, states = states, dictionary = dictionary, results = Dict.empty} 

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
            case state.action of
                LogAction message ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "log" )
                            , ( "message", Encode.string message)
                            ]
                        )

                ClickAction selector description ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "click" )
                            , ( "selector", Encode.string selector )
                            , ( "description", Encode.string description )
                            ]
                        )

                RetrieveAction key selector description pattern ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "retrieve" )
                            , ( "key", Encode.string key )
                            , ( "selector", Encode.string selector )
                            , ( "description", Encode.string description )
                            , ( "pattern", maybeEncode Encode.string pattern )
                            ]
                        )

                WaitAction selector timeout retries description textContent ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "wait" )
                            , ( "selector", Encode.string selector )
                            , ( "timeout", Encode.int timeout )
                            , ( "retries", Encode.int retries )
                            , ( "description", Encode.string description )
                            , ( "textContent", maybeEncode Encode.string textContent )
                            ]
                        )

                WaitForUrlAction urlPattern timeout retries description ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "waitForUrl" )
                            , ( "urlPattern", Encode.string urlPattern )
                            , ( "timeout", Encode.int timeout )
                            , ( "retries", Encode.int retries )
                            , ( "description", Encode.string description )
                            ]
                        )

                NestedClickAction parentSelector childSelector description ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "nestedClick" )
                            , ( "parentSelector", Encode.string parentSelector )
                            , ( "childSelector", Encode.string childSelector )
                            , ( "description", Encode.string description )
                            ]
                        )

                ClickByTextAction tag text description ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "clickByText" )
                            , ( "tag", Encode.string tag )
                            , ( "text", Encode.string text )
                            , ( "description", Encode.string description )
                            ]
                        )

                FillValueAction key ->
                    let
                        value =
                            Dict.get key executingModel.dictionary
                                |> Maybe.withDefault ""
                    in
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "fillValue" )
                            , ( "value", Encode.string value )
                            ]
                        )

                FillChildPValueAction key ->
                    let
                        value =
                            Dict.get key executingModel.dictionary
                                |> Maybe.withDefault ""
                    in
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "fillChildPValue" )
                            , ( "value", Encode.string value )
                            ]
                        )

                FocusElementAction selector description ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "focusElement" )
                            , ( "selector", Encode.string selector )
                            , ( "description", Encode.string description )
                            ]
                        )

                ProcessingFinishedAction ->
                    sendAction
                        (Encode.object
                            [ ( "type", Encode.string "processingFinished" )
                            ]
                        )

        Nothing ->
            Cmd.none

sendAction : Value -> Cmd Msg
sendAction value =
    Ports.sendMessageToContentScript value

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
