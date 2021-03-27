module Server.Player exposing (Player, decoder)

import Json.Decode


type alias Player =
    { id : String
    , name : String
    }


decoder : Json.Decode.Decoder Player
decoder =
    Json.Decode.map2 Player
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
