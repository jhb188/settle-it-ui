module Server.State exposing (State, Status(..), decoder)

import BodyData
import Json.Decode
import Physics.Body
import Server.Body
import Server.Player
import Server.Team


type Status
    = Pending
    | Playing
    | Finished


type alias State =
    { id : String
    , topic : String
    , players : List Server.Player.Player
    , teams : List Server.Team.Team
    , bodies : List (Physics.Body.Body BodyData.Data)
    , lastUpdated : Int
    , status : Status
    }


statusDecoder : Json.Decode.Decoder Status
statusDecoder =
    Json.Decode.map
        (\s ->
            case s of
                "pending" ->
                    Pending

                "playing" ->
                    Playing

                "finished" ->
                    Finished

                _ ->
                    Finished
        )
        Json.Decode.string


decoder : String -> Json.Decode.Decoder State
decoder playerId =
    Json.Decode.map7 State
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "topic" Json.Decode.string)
        (Json.Decode.field "players" (Json.Decode.list Server.Player.decoder))
        (Json.Decode.field "teams" (Json.Decode.list Server.Team.decoder))
        (Json.Decode.field "bodies" (Server.Body.physicsBodiesDecoder playerId))
        (Json.Decode.field "last_updated" Json.Decode.int)
        (Json.Decode.field "status" statusDecoder)
