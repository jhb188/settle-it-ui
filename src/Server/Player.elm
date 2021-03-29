module Server.Player exposing (Player, decoder)

import Json.Decode


type alias Player =
    { id : String
    , name : String
    , teamId : Maybe String
    }


decoder : Json.Decode.Decoder Player
decoder =
    Json.Decode.map3 Player
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "team_id" (Json.Decode.maybe Json.Decode.string))
