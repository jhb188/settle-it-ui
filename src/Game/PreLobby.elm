port module Game.PreLobby exposing (..)

import Common exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Model =
    { gameCode : String
    , topic : String
    , playerId : PlayerId
    }


type Msg
    = GameCodeInputUpdated String
    | TopicUpdated String
    | JoinGame
    | CreateGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg preLobbyState =
    case msg of
        GameCodeInputUpdated newGameCode ->
            ( { preLobbyState | gameCode = String.toLower newGameCode }, Cmd.none )

        TopicUpdated newTopicName ->
            ( { preLobbyState | topic = newTopicName }, Cmd.none )

        JoinGame ->
            ( preLobbyState, joinGame { playerId = preLobbyState.playerId, gameId = Just preLobbyState.gameCode, topic = Nothing } )

        CreateGame ->
            ( preLobbyState, joinGame { playerId = preLobbyState.playerId, gameId = Nothing, topic = Just preLobbyState.topic } )


view : Model -> Html Msg
view preLobbyState =
    Html.div
        [ Html.Attributes.style "flex" "1"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "space-around"
        , Html.Attributes.style "height" "90%"
        , Html.Attributes.style "display" "flex"
        ]
        [ Html.div
            [ Html.Attributes.class "flex-column-item"
            ]
            [ Html.h2 [] [ Html.text "Join a debate topic" ]
            , Html.div []
                [ Html.input
                    [ Html.Events.onInput GameCodeInputUpdated
                    , Html.Attributes.value preLobbyState.gameCode
                    , Html.Attributes.placeholder "Enter a game code"
                    ]
                    []
                , Html.button
                    [ Html.Attributes.disabled (String.length preLobbyState.gameCode /= 4)
                    , Html.Events.onClick JoinGame
                    ]
                    [ Html.text "Join" ]
                ]
            ]
        , Html.div
            [ Html.Attributes.class "flex-column-item"
            ]
            [ Html.h1 [] [ Html.text "OR" ]
            ]
        , Html.div
            [ Html.Attributes.class "flex-column-item"
            ]
            [ Html.h2 [] [ Html.text "Start a new debate topic" ]
            , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text "(like \"Where should we eat tonight?\")" ]
            , Html.div []
                [ Html.textarea
                    [ Html.Events.onInput TopicUpdated
                    , Html.Attributes.value preLobbyState.topic
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.placeholder "Enter a topic"
                    ]
                    []
                ]
            , Html.button
                [ Html.Attributes.disabled (String.length preLobbyState.topic == 0)
                , Html.Events.onClick CreateGame
                ]
                [ Html.text "Start" ]
            ]
        ]


port joinGame : { playerId : String, gameId : Maybe String, topic : Maybe String } -> Cmd msg
