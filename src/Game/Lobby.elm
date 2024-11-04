port module Game.Lobby exposing (..)

import Color
import Common exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Server.Player
import Server.State
import Server.Team


type alias Model =
    { playerId : PlayerId
    , teams : List Server.Team.Team
    , players : List Server.Player.Player
    , topic : String
    , gameId : String
    , teamNameInput : String
    , isEditingPlayerName : Bool
    , playerNameInput : String
    }


type Msg
    = TeamNameInputUpdated String
    | CreateTeam
    | EditPlayerName
    | SavePlayerName
    | PlayerNameInputUpdated String
    | JoinTeam String
    | DeleteTeam String
    | StartGame
    | LobbyUpdated Server.State.State


init : PlayerId -> Server.State.State -> ( Model, Cmd Msg )
init playerId serverState =
    ( { playerId = playerId
      , players = serverState.players
      , teams = serverState.teams
      , topic = serverState.topic
      , gameId = serverState.id
      , teamNameInput = ""
      , playerNameInput =
            case List.Extra.find (.id >> (==) playerId) serverState.players of
                Just player ->
                    player.name

                Nothing ->
                    ""
      , isEditingPlayerName = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg lobbyState =
    case msg of
        TeamNameInputUpdated newTeamName ->
            ( { lobbyState | teamNameInput = newTeamName }, Cmd.none )

        CreateTeam ->
            ( { lobbyState | teamNameInput = "" }, createTeam { playerId = lobbyState.playerId, name = lobbyState.teamNameInput } )

        LobbyUpdated nextGameData ->
            ( { lobbyState | players = nextGameData.players, teams = nextGameData.teams }, Cmd.none )

        EditPlayerName ->
            ( { lobbyState | isEditingPlayerName = True }, Cmd.none )

        PlayerNameInputUpdated name ->
            ( { lobbyState | playerNameInput = name }, Cmd.none )

        SavePlayerName ->
            ( { lobbyState | isEditingPlayerName = False }
            , updatePlayerName
                { playerId = lobbyState.playerId
                , name = lobbyState.playerNameInput
                }
            )

        JoinTeam teamId ->
            ( lobbyState, joinTeam { playerId = lobbyState.playerId, teamId = teamId } )

        DeleteTeam teamId ->
            ( lobbyState, deleteTeam teamId )

        StartGame ->
            ( lobbyState, startGame () )


view : Model -> Html Msg
view lobbyState =
    let
        unassignedPlayers =
            List.filter
                (\p ->
                    case p.teamId of
                        Nothing ->
                            True

                        Just _ ->
                            False
                )
                lobbyState.players

        playersForTeam team =
            List.filter
                (\p -> p.teamId == Just team.id)
                lobbyState.players

        onTeam team currentPlayerId =
            List.member currentPlayerId (List.map .id <| playersForTeam team)

        viewPlayer player =
            let
                playerName =
                    case player.name of
                        "" ->
                            Html.span [ Html.Attributes.style "font-style" "italic" ] [ Html.text "Anonymous Player" ]

                        name ->
                            Html.span [] [ Html.text name ]
            in
            if player.id == lobbyState.playerId then
                if lobbyState.isEditingPlayerName then
                    Html.div []
                        [ Html.input
                            [ Html.Attributes.value lobbyState.playerNameInput
                            , Html.Events.onInput PlayerNameInputUpdated
                            ]
                            []
                        , Html.button [ Html.Events.onClick SavePlayerName ]
                            [ Html.text "Save"
                            ]
                        ]

                else
                    Html.div []
                        [ playerName
                        , Html.button [ Html.Events.onClick EditPlayerName ]
                            [ Html.text "Edit"
                            ]
                        ]

            else
                Html.div []
                    [ playerName
                    ]
    in
    Html.div []
        [ Html.h1 [] [ Html.text lobbyState.gameId ]
        , Html.h3 [] [ Html.text lobbyState.topic ]
        , Html.div []
            [ Html.button
                [ Html.Attributes.disabled (List.length unassignedPlayers > 0)
                , Html.Events.onClick StartGame
                ]
                [ Html.text "Start Game" ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.placeholder "Team name"
                , Html.Attributes.value lobbyState.teamNameInput
                , Html.Events.onInput TeamNameInputUpdated
                ]
                []
            , Html.button
                [ Html.Events.onClick CreateTeam
                , Html.Attributes.disabled (String.length lobbyState.teamNameInput == 0)
                ]
                [ Html.text "Create" ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-flow" "row wrap"
            ]
            (List.map
                (\team ->
                    let
                        players =
                            playersForTeam team
                    in
                    Html.div
                        [ Html.Attributes.style "flex" "1"
                        , Html.Attributes.style "box-shadow" "0 2px 4px 0 rgba(0,0,0,0.2)"
                        , Html.Attributes.style "margin" "5px"
                        , Html.Attributes.style "min-width" "150px"
                        , Html.Attributes.style "padding" "20px"
                        , Html.Attributes.style "border" ("2px solid " ++ Color.toCssString team.color)
                        ]
                        [ Html.div
                            []
                            [ Html.h4 [ Html.Attributes.style "margin" "5px" ] [ Html.text team.cause ]
                            , Html.div [ Html.Attributes.style "min-height" "20px" ]
                                [ if not (onTeam team lobbyState.playerId) then
                                    Html.button [ Html.Events.onClick (JoinTeam team.id) ] [ Html.text "Join" ]

                                  else
                                    Html.span [] []
                                , if List.isEmpty players && team.ownerId == lobbyState.playerId then
                                    Html.button [ Html.Events.onClick (DeleteTeam team.id) ] [ Html.text "Delete" ]

                                  else
                                    Html.span [] []
                                ]
                            , Html.div [] (List.map viewPlayer players)
                            ]
                        ]
                )
                lobbyState.teams
                ++ [ Html.div
                        [ Html.Attributes.style "flex" "1"
                        , Html.Attributes.style "box-shadow" "0 2px 4px 0 rgba(0,0,0,0.2)"
                        , Html.Attributes.style "margin" "5px"
                        , Html.Attributes.style "min-width" "150px"
                        , Html.Attributes.style "padding" "20px"
                        ]
                        [ Html.div
                            []
                            [ Html.h4 [ Html.Attributes.style "margin" "5px" ]
                                [ Html.text "Unassigned"
                                ]
                            , Html.div []
                                (List.map viewPlayer unassignedPlayers)
                            ]
                        ]
                   ]
            )
        ]


port createTeam : { playerId : String, name : String } -> Cmd msg


port joinTeam : { playerId : String, teamId : String } -> Cmd msg


port deleteTeam : String -> Cmd msg


port updatePlayerName : { playerId : String, name : String } -> Cmd msg


port startGame : () -> Cmd msg
