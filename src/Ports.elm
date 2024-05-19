port module Ports exposing (sendMessageToContentScript, receiveMessageFromContentScript)

import Json.Decode exposing (Value)
import Json.Encode exposing (Value)

port sendMessageToContentScript : Value -> Cmd msg

port receiveMessageFromContentScript : (Value -> msg) -> Sub msg
