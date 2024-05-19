module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, form, input, label, text, textarea)
import Html.Attributes exposing (placeholder, style, name, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Dict exposing (Dict)
import StateMachine exposing (Action(..), State, StateMachineModel, init, subscriptions, update)
import Tuple exposing (first, second)


-- MODEL

type alias Flags =
    { url : String
    , title : String
    , lang : String
    }

type Model
    = Form { title : String, description : String }
    | Executing StateMachineModel
    | Results (Dict String String)
    | Error String


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Form { title = "", description = "" }
    , Cmd.none
    )

-- UPDATE

type Msg
    = StateMachineMsg StateMachine.Msg
    | SubmitForm
    | UpdateTitle String
    | UpdateDescription String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StateMachineMsg smMsg ->
            case model of
                Executing stateMachine ->
                    let
                        ( newStateMachine, smCmd ) =
                            StateMachine.update smMsg stateMachine
                    in
                    case StateMachine.results newStateMachine of
                        StateMachine.ExecutionStillRunning { results } ->
                            ( Executing newStateMachine , Cmd.map StateMachineMsg smCmd)

                        StateMachine.ExecutionSucceeded { results  } ->
                            ( Results results, Cmd.none )

                        StateMachine.ExecutionFailed { error  } ->
                            ( Error error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitForm ->
            case model of
                Form { title, description } ->
                  let
                    initialStates =
                        [ { action = LogAction "Checking if the current URL is a Facebook group", nextState = 1 }
                        , { action = WaitForUrlAction "facebook.com/groups" 0 1 "Wait for FB group URL", nextState = 2 }
                        , { action = ClickAction "div.xmjcpbm.x2g32xy[role='button'][tabindex='0']" "Click first button", nextState = 3 }
                        , { action = WaitAction "form[method='POST']:not([class]):not([action])" 1000 5 "Wait for post form" Nothing, nextState = 4 }
                        , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "div[aria-label='Mehr']" "Click 'Mehr' within post form", nextState = 5 }
                        , { action = WaitAction "span" 1000 5 "Wait for 'Live-Video' span to appear" (Just "Live-Video"), nextState = 6 }
                        , { action = ClickByTextAction "span" "Live-Video" "Click 'Live-Video' span", nextState = 7 }
                        , { action = WaitForUrlAction "/live/producer" 1000 30 "Wait for URL change to /live/producer", nextState = 8 }
                        , { action = WaitAction "span" 1000 5 "Wait for 'Live gehen' span to appear" (Just "Live gehen"), nextState = 9 }
                        , { action = ClickByTextAction "span" "Live gehen" "Click 'Live gehen' span", nextState = 10 }
                        , { action = WaitAction "div[aria-label='Streaming-Software']" 1000 10 "Wait for 'Streaming-Software' div to appear" Nothing, nextState = 11 }
                        , { action = ClickAction "div[aria-label='Streaming-Software']" "Click 'Streaming-Software' div", nextState = 12 }
                        , { action = RetrieveAction "streamKey" "input[aria-label='Stream-Schlüssel']" "Retrieve stream key" Nothing, nextState = 13 }
                        , { action = ClickByTextAction "span" "Worum geht es in deinem Live-Video?" "Click 'Worum geht es in deinem Live-Video?' span", nextState = 14 }
                        , { action = WaitAction "span" 1000 5 "Wait for 'Titel (erforderlich)' span to appear" (Just "Titel \\(erforderlich\\)"), nextState = 15 }
                        , { action = ClickByTextAction "span" "Titel \\(erforderlich\\)" "Click 'Titel (erforderlich)' span", nextState = 16 }
                        , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "input[type='text']" "Click 'Titel' within dialog", nextState = 17 }
                        , { action = FillValueAction "title", nextState = 18 }
                        , { action = FocusElementAction "div[aria-label='Beschreibung (erforderlich)'][role='textbox']" "Description field", nextState = 19 }
                        , { action = FillChildPValueAction "description", nextState = 20 }
                        , { action = NestedClickAction "form[method='POST']:not([class]):not([action])" "input[type='text']" "Click 'Titel' within dialog", nextState = 21 }
                        , { action = ClickAction "div[aria-label='Speichern'][role='button']" "Click 'Speichern' div", nextState = 22 }
                        , { action = ClickByTextAction "span" "Erweiterte Einstellungen" "Click 'Erweiterte Einstellungen' span", nextState = 23 }
                        , { action = RetrieveAction "serverUrl" "input[type='text']" "Retrieve server URL" (Just "rtmps"), nextState = 24 }
                        , { action = ProcessingFinishedAction, nextState = -1 }
                        ]

                    ( newStateMachine, newCmd ) =
                        StateMachine.init initialStates (Dict.fromList [ ("title", title), ("description", description) ])
                 in
                 ( Executing newStateMachine 
                 , Cmd.batch [ Cmd.map StateMachineMsg newCmd ]
                 )
                _ ->
                    ( model, Cmd.none )


        UpdateTitle newTitle ->
            case model of
                Form { title, description } ->
                    ( Form { title = newTitle, description = description }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateDescription newDescription ->
            case model of
                Form { title, description } ->
                    ( Form { title = title, description = newDescription }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Form { title, description } ->
          div []
            [ form [ onSubmit SubmitForm, style "padding" "20px", style "background-color" "#f0f0f0", name "plugin-form" ]
                [ div [ style "margin-bottom" "20px" ]
                    [ label [] [ text "Title: " ]
                    , input [ type_ "text", value title, onInput UpdateTitle, style "width" "100%" ] []
                    ]
                , div [ style "margin-bottom" "20px" ]
                    [ label [] [ text "Description: " ]
                    , textarea [ value description, onInput UpdateDescription, placeholder "Enter description here...", style "width" "100%", style "height" "100px" ] []
                    ]
                , button [ onClick SubmitForm ] [ text "Submit" ]
                ]
            ]

        Executing stateMachine ->
            div [] [ text "Executing..." ]

        Results results ->
            div [] (Dict.toList results |> List.map viewResult)

        Error error ->
            div [] [ text error ]


viewResult : (String, String) -> Html Msg
viewResult (key, value) =
    div []
        [ text (key ++ ": ")
        , text value
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Executing stateMachine ->
            Sub.map StateMachineMsg (StateMachine.subscriptions stateMachine)

        _ ->
            Sub.none

-- MAIN

main =
    Browser.element
        { init = \flags -> init flags
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
