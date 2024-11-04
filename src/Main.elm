port module Main exposing (main)

import BodyData exposing (Class(..), Dimensions(..))
import Browser
import Browser.Events
import Common exposing (..)
import Game
import Game.Lobby
import Game.Playing
import Game.PostGame
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Server.State


type alias Flags =
    { playerId : PlayerId
    }


type alias Model =
    ( PlayerId, Game.Model )


type Msg
    = ServerUpdate Json.Encode.Value
    | ClientUpdate Game.Msg


init : Flags -> ( Model, Cmd Msg )
init { playerId } =
    ( ( playerId
      , Game.init playerId
      )
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( playerId, game ) =
    let
        ( nextGame, nextCmd ) =
            case msg of
                ServerUpdate gameUpdateData ->
                    case Json.Decode.decodeValue (Server.State.decoder playerId) gameUpdateData of
                        Err _ ->
                            ( game, Cmd.none )

                        Ok serverState ->
                            case ( game, serverState.status ) of
                                ( Game.PreLobby _, _ ) ->
                                    Tuple.mapBoth Game.Lobby (Cmd.map Game.LobbyMsg) <|
                                        Game.Lobby.init playerId serverState

                                ( Game.Lobby _, Server.State.Playing ) ->
                                    Tuple.mapBoth Game.Playing (Cmd.map Game.PlayingMsg) <|
                                        Game.Playing.init playerId serverState

                                ( Game.Lobby lobbyState, _ ) ->
                                    let
                                        ( nextLobbyState, cmds ) =
                                            Game.Lobby.update (Game.Lobby.LobbyUpdated serverState) lobbyState
                                    in
                                    ( Game.Lobby nextLobbyState, Cmd.map Game.LobbyMsg cmds )

                                ( Game.Playing _, Server.State.Finished ) ->
                                    Tuple.mapBoth Game.PostGame (Cmd.map Game.PostGameMsg) <|
                                        Game.PostGame.init playerId serverState

                                ( Game.Playing playingState, _ ) ->
                                    ( Game.Playing playingState, Cmd.none )

                                ( Game.PostGame _, _ ) ->
                                    Tuple.mapBoth Game.PostGame (Cmd.map Game.PostGameMsg) <|
                                        Game.PostGame.init playerId serverState

                ClientUpdate clientUpdateMsg ->
                    Game.update clientUpdateMsg game
    in
    ( ( playerId, nextGame ), Cmd.map ClientUpdate <| nextCmd )


view : Model -> Html Msg
view ( _, game ) =
    Html.map ClientUpdate <| Game.view game


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ case model of
                        ( _, Game.Playing _ ) ->
                            Browser.Events.onAnimationFrameDelta (ClientUpdate << Game.PlayingMsg << Game.Playing.Delta)

                        _ ->
                            Sub.none
                    , gameUpdated ServerUpdate
                    , bodiesUpdated (ClientUpdate << Game.PlayingMsg << Game.Playing.BodiesUpdated)
                    ]
        , update = update
        }


port gameUpdated : (Json.Encode.Value -> msg) -> Sub msg


port bodiesUpdated : (Json.Encode.Value -> msg) -> Sub msg
