module Game exposing (..)

import Common exposing (..)
import Game.Lobby as Lobby
import Game.Playing as Playing
import Game.PostGame as PostGame
import Game.PreLobby as PreLobby
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import List.Extra


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
            Html.map PlayingMsg <|
                Html.Lazy.lazy5 Playing.view
                    playingState.world
                    playingState.teams
                    playingState.cameraXRotationAngle
                    playingState.cameraZRotationAngle
                    playingState.viewSize

        PostGame postGameState ->
            let
                playersForTeam team =
                    List.filter (\( p, _ ) -> p.teamId == Just team.id) postGameState.players

                playerIsAlive ( _, stats ) =
                    stats.hp > 0

                maybeWinningTeam =
                    List.Extra.find (playersForTeam >> List.filter playerIsAlive >> List.length >> (\l -> l > 0)) postGameState.teams

                teamHp team =
                    team |> playersForTeam |> List.foldl (\( _, pStats ) -> (+) pStats.hp) 0

                teamsByHp =
                    List.reverse <| List.sortBy teamHp postGameState.teams

                viewPlayer ( p, pStats ) =
                    Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "row"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "justify-content" "center"
                        ]
                        [ case p.name of
                            "" ->
                                Html.span [ Html.Attributes.style "font-style" "italic" ] [ Html.text "Anonymous Player" ]

                            name ->
                                Html.text name
                        , viewHp pStats.hp
                        ]

                viewHp hp =
                    Html.div
                        [ Html.Attributes.style "margin-left" "5px"
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "row"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "justify-content" "center"
                        ]
                        (case hp of
                            0 ->
                                [ Html.text "💀" ]

                            n ->
                                viewHpHelper n
                        )

                viewHpHelper hp =
                    case hp of
                        0 ->
                            []

                        n ->
                            Html.div
                                [ Html.Attributes.style "border-radius" "100%"
                                , Html.Attributes.style "height" "8px"
                                , Html.Attributes.style "width" "8px"
                                , Html.Attributes.style "margin-left" "3px"
                                , Html.Attributes.style "background-color" "green"
                                ]
                                []
                                :: viewHpHelper (n - 1)
            in
            Html.div []
                [ Html.h3 []
                    [ Html.text postGameState.topic
                    ]
                , Html.h2 []
                    [ Html.text
                        (case maybeWinningTeam of
                            Just winningTeam ->
                                winningTeam.cause

                            Nothing ->
                                "draw"
                        )
                    ]
                , Html.div []
                    (teamsByHp
                        |> List.map
                            (\team ->
                                Html.div
                                    []
                                    (Html.h4 [] [ Html.text team.cause ]
                                        :: (team
                                                |> playersForTeam
                                                |> List.map viewPlayer
                                           )
                                    )
                            )
                    )
                ]
