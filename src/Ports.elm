port module Ports exposing (loadedGame, saveGame)

{-| Port module for JavaScript interop (localStorage save/load).
-}

import Json.Encode as Encode


{-| Send game state to JavaScript for saving to localStorage.
-}
port saveGame : Encode.Value -> Cmd msg


{-| Receive saved game state from JavaScript on app startup.
-}
port loadedGame : (Encode.Value -> msg) -> Sub msg
