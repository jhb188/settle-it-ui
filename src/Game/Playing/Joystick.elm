module Game.Playing.Joystick exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type JoystickMsg
    = JoystickDown Vec2
    | JoystickMove Vec2
    | JoystickUp


type alias Model =
    Maybe
        { initialPos : Vec2
        , currentOffset : Vec2
        }


init : Model
init =
    Nothing


update : Model -> JoystickMsg -> Model
update maybeModel msg =
    case ( msg, maybeModel ) of
        ( JoystickDown pos, _ ) ->
            Just { currentOffset = pos, initialPos = pos }

        ( JoystickMove pos, Just model ) ->
            Just { model | currentOffset = pos }

        ( JoystickMove _, Nothing ) ->
            Nothing

        ( JoystickUp, _ ) ->
            init


view :
    Float
    -> (JoystickMsg -> msg)
    -> Model
    -> Html msg
view viewSize onAction maybeModel =
    let
        { x, y } =
            modelToNubPosition viewSize maybeModel
    in
    Html.div
        [ Html.Attributes.style "touch-action" "none"
        , Html.Attributes.style "user-select" "none"
        , Html.Attributes.style "-webkit-user-select" "none"
        , Html.Attributes.width (round viewSize)
        , Html.Attributes.style "height" "0"
        , Html.Attributes.style "padding-bottom" "50%"
        , Html.Attributes.style "flex" "1"
        , Html.Attributes.style "margin" "5px"
        , Html.Attributes.class "joystick"
        , Pointer.onDown (\event -> onAction <| JoystickDown <| tupleToVec2 event.pointer.offsetPos)
        , Pointer.onMove (\event -> onAction <| JoystickMove <| tupleToVec2 event.pointer.offsetPos)
        , Pointer.onUp (onAction << always JoystickUp)
        ]
        [ Html.div
            [ Html.Attributes.class "joystick-nub"
            , Html.Attributes.style "left" x
            , Html.Attributes.style "top" y
            ]
            []
        ]


tupleToVec2 : ( Float, Float ) -> Vec2
tupleToVec2 ( x, y ) =
    vec2 x (negate y)


modelToNubPosition : Float -> Model -> { x : String, y : String }
modelToNubPosition viewSize maybeModel =
    let
        center =
            viewSize / 2

        renderPos n =
            let
                percentOffset =
                    (n * 100 / viewSize) |> round

                clampedOffset =
                    clamp 0 100 percentOffset
            in
            clampedOffset |> String.fromInt |> (\s -> s ++ "%")
    in
    case maybeModel of
        Just { initialPos, currentOffset } ->
            let
                initialPosCoords =
                    Vec2.toRecord initialPos

                currentOffsetCoords =
                    Vec2.toRecord currentOffset
            in
            { x = renderPos <| currentOffsetCoords.x - initialPosCoords.x + center
            , y = renderPos <| negate (currentOffsetCoords.y - initialPosCoords.y) + center
            }

        Nothing ->
            { x = renderPos center, y = renderPos center }


getDirection : Model -> Maybe { x : Float, y : Float }
getDirection =
    Maybe.map
        (\m ->
            Vec2.direction m.currentOffset m.initialPos
                |> Vec2.toRecord
        )
