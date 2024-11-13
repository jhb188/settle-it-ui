module Game.PostGame exposing (..)

import Common exposing (..)
import Html exposing (Html)
import Html.Attributes
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


view : Model -> Html Msg
view postGameState =
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
                ]
                [ Html.div
                    [ Html.Attributes.style "flex" "1"
                    ]
                    [ case p.name of
                        "" ->
                            Html.span [ Html.Attributes.style "font-style" "italic" ]
                                [ Html.text "Anonymous Player" ]

                        name ->
                            Html.text name
                    ]
                , viewHp pStats.hp
                ]

        viewHp hp =
            Html.div
                [ Html.Attributes.style "margin-left" "5px"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "row"
                , Html.Attributes.style "flex" "1"
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
    Html.div
        [ Html.Attributes.style "flex" "1"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "space-around"
        , Html.Attributes.style "height" "90%"
        , Html.Attributes.style "display" "flex"
        ]
        [ Html.div []
            [ Html.h3 []
                [ Html.text postGameState.topic
                ]
            , Html.h2 [ Html.Attributes.class "soda-bubble" ]
                [ Html.text
                    (case maybeWinningTeam of
                        Just winningTeam ->
                            winningTeam.cause

                        Nothing ->
                            "draw"
                    )
                ]
            ]
        , Html.div [ Html.Attributes.style "width" "100%" ]
            (teamsByHp
                |> List.map
                    (\team ->
                        Html.div
                            [ Html.Attributes.style "padding" "10px" ]
                            (Html.h3 [ Html.Attributes.style "padding" "10px" ]
                                [ Html.text team.cause ]
                                :: (team
                                        |> playersForTeam
                                        |> List.map viewPlayer
                                   )
                            )
                    )
            )
        ]
