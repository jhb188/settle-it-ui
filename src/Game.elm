module Game exposing (..)

import Common exposing (..)
import Game.Lobby as Lobby
import Game.Playing as Playing
import Game.PostGame as PostGame
import Game.PreLobby as PreLobby
import Html exposing (Html)


type Model
    = PreLobby PreLobby.Model
    | Lobby Lobby.Model
    | Playing Playing.Model
    | PostGame PostGame.Model


type Msg
    = PreLobbyMsg PreLobby.Msg
    | LobbyMsg Lobby.Msg
    | PlayingMsg Playing.Msg
    | PostGameMsg PostGame.Msg


init : PlayerId -> Model
init playerId =
    PreLobby
        { gameCode = ""
        , topic = ""
        , playerId = playerId
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PreLobbyMsg preLobbyMsg, PreLobby preLobbyState ) ->
            let
                ( nextPreLobbyState, cmds ) =
                    PreLobby.update preLobbyMsg preLobbyState
            in
            ( PreLobby nextPreLobbyState, Cmd.map PreLobbyMsg cmds )

        ( LobbyMsg lobbyMsg, Lobby lobbyState ) ->
            let
                ( nextLobbyState, cmds ) =
                    Lobby.update lobbyMsg lobbyState
            in
            ( Lobby nextLobbyState, Cmd.map LobbyMsg cmds )

        ( PlayingMsg playingMsg, Playing playingState ) ->
            let
                ( nextPlayingState, cmds ) =
                    Playing.update playingMsg playingState
            in
            ( Playing nextPlayingState, Cmd.map PlayingMsg cmds )

        ( _, PostGame postGameState ) ->
            ( PostGame postGameState, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view game =
    case game of
        PreLobby preLobbyState ->
            Html.map PreLobbyMsg <| PreLobby.view preLobbyState

        Lobby lobbyState ->
            Html.map LobbyMsg <| Lobby.view lobbyState

        Playing playingState ->
            Html.map PlayingMsg <| Playing.view playingState

        PostGame postGameState ->
            Html.map PostGameMsg <| PostGame.view postGameState
