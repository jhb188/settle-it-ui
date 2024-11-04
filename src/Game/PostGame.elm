module Game.PostGame exposing (..)

import Common exposing (..)
import List.Extra
import Physics.Body
import Server.Player
import Server.State
import Server.Team


type alias Model =
    { playerId : PlayerId
    , teams : List Server.Team.Team
    , players : List ( Server.Player.Player, PlayerStats )
    , topic : String
    }


type Msg
    = NoOp


type alias PlayerStats =
    { hp : Int }


init : PlayerId -> Server.State.State -> ( Model, Cmd Msg )
init playerId serverState =
    let
        playersWithStats =
            List.filterMap
                (\body ->
                    let
                        { id, hp } =
                            Physics.Body.data body
                    in
                    serverState.players
                        |> List.Extra.find (.id >> (==) id)
                        |> Maybe.map (\player -> ( player, { hp = hp } ))
                )
                serverState.bodies
    in
    ( { playerId = playerId
      , teams = serverState.teams
      , players = playersWithStats
      , topic = serverState.topic
      }
    , Cmd.none
    )
