port module Ports exposing (sendAction, receiveMessageFromContentScript)

import Json.Decode exposing (Value)
import Json.Encode as Encode exposing (Value)

port sendMessageToContentScript : Value -> Cmd msg

port receiveMessageFromContentScript : (Value -> msg) -> Sub msg

sendAction : String -> List (String, Encode.Value) -> Cmd msg
sendAction type_ records =
    (Encode.object
        (( "type", Encode.string type_ ) :: records)
    )
    |> sendMessageToContentScript
