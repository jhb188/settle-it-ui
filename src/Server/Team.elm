module Server.Team exposing (Team, decoder)

import Color
import Json.Decode
import Set


type alias Team =
    { id : String
    , ownerId : String
    , cause : String
    , playerIds : Set.Set String
    , color : Color.Color
    }


playerIdsDecoder : Json.Decode.Decoder (Set.Set String)
playerIdsDecoder =
    Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string)


colorDecoder : Json.Decode.Decoder Color.Color
colorDecoder =
    Json.Decode.map
        (\colorString ->
            case colorString of
                "red" ->
                    Color.red

                "orange" ->
                    Color.orange

                "yellow" ->
                    Color.yellow

                "green" ->
                    Color.green

                "blue" ->
                    Color.blue

                "purple" ->
                    Color.purple

                "brown" ->
                    Color.brown

                "light-red" ->
                    Color.red

                "light-orange" ->
                    Color.lightOrange

                "light-yellow" ->
                    Color.lightYellow

                "light-green" ->
                    Color.lightGreen

                "light-blue" ->
                    Color.lightBlue

                "light-purple" ->
                    Color.lightPurple

                "light-brown" ->
                    Color.lightBrown

                "dark-red" ->
                    Color.darkRed

                "dark-orange" ->
                    Color.darkOrange

                "dark-yellow" ->
                    Color.darkYellow

                "dark-green" ->
                    Color.darkGreen

                "dark-blue" ->
                    Color.darkBlue

                "dark-purple" ->
                    Color.darkPurple

                "dark-brown" ->
                    Color.darkBrown

                _ ->
                    Color.black
        )
        Json.Decode.string


decoder : Json.Decode.Decoder Team
decoder =
    Json.Decode.map5 Team
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "owner_id" Json.Decode.string)
        (Json.Decode.field "cause" Json.Decode.string)
        (Json.Decode.field "player_ids" playerIdsDecoder)
        (Json.Decode.field "color" colorDecoder)