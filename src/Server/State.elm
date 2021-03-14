module Server.State exposing (State, decoder)

import BodyData
import Json.Decode
import Physics.Body
import Server.Body


type alias State =
    { bodies : List (Physics.Body.Body BodyData.Data)
    , lastUpdated : Int
    }


decoder : String -> Json.Decode.Decoder State
decoder playerId =
    Json.Decode.map2 State
        (Json.Decode.field "bodies" (Server.Body.physicsBodiesDecoder playerId))
        (Json.Decode.field "last_updated" Json.Decode.int)
