module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL


type alias Flags =
    ()


type alias Model =
    { viewSize : Float
    , me : Person
    , movementPadOrigin : Vec2
    , movementPadOffset : Maybe Vec2
    , movementPadHeldMs : Float
    , targetPadOrigin : Vec2
    , targetPadOffset : Maybe Vec2
    , targetPadHeldMs : Float
    }


type Msg
    = Delta Float
    | ViewportChanged Browser.Dom.Viewport
    | MovementPadDown Vec2
    | MovementPadMoved Vec2
    | MovementPadUp
    | TargetPadDown Vec2
    | TargetPadMoved Vec2
    | TargetPadUp


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        mePos =
            vec3 0 0 5

        me =
            { center = mePos
            , color = vec3 122 122 0
            , eyeDirection = Vec3.direction mePos (vec3 0 0 0)
            , yVelocity = 0
            }
    in
    ( { viewSize = 0
      , me = me
      , movementPadOrigin = vec2 0 0
      , movementPadOffset = Nothing
      , movementPadHeldMs = 0
      , targetPadOrigin = vec2 0 0
      , targetPadOffset = Nothing
      , targetPadHeldMs = 0
      }
    , Task.perform ViewportChanged Browser.Dom.getViewport
    )


movementScale : Float
movementScale =
    0.05


jumpThresholdMs : Float
jumpThresholdMs =
    300.0


shootThresholdMs : Float
shootThresholdMs =
    300.0


isValidPos : Vec3 -> Bool
isValidPos vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
    not (isNaN x || isNaN y || isNaN z)


floorY : Float
floorY =
    0


isOnFloor : Vec3 -> Bool
isOnFloor vec =
    Vec3.getY vec <= floorY


clampToFloor : Vec3 -> Vec3
clampToFloor vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
    if y < 0 then
        vec3 x 0 z

    else
        vec


gravityPerMs : Float
gravityPerMs =
    -9.8 / 1000


jumpVelocity : Float
jumpVelocity =
    8.0


updateY : Float -> Model -> Model
updateY ms model =
    let
        { me } =
            model

        yChangeVec =
            vec3 0 (me.yVelocity * (ms / 1000)) 0

        nextMePos =
            me.center |> Vec3.add yChangeVec |> clampToFloor

        yDiff =
            Vec3.getY nextMePos - Vec3.getY me.center

        nextTargetPos =
            Vec3.add me.eyeDirection (vec3 0 yDiff 0)

        nextYVelocity =
            me.yVelocity + (ms * gravityPerMs)
    in
    if isValidPos nextTargetPos && isValidPos nextMePos && (not <| isNaN nextYVelocity) then
        { model | me = { me | center = nextMePos, yVelocity = nextYVelocity, eyeDirection = nextTargetPos } }

    else
        model


updateMovement : Float -> Model -> Model
updateMovement ms model =
    case model.movementPadOffset of
        Just relativePos ->
            let
                { me } =
                    model

                direction =
                    Vec2.direction relativePos model.movementPadOrigin

                { x, y } =
                    Vec2.toRecord <| direction

                orientation =
                    Vec3.setY 0 <| Vec3.direction me.eyeDirection me.center

                up =
                    vec3 0 1 0

                perp =
                    Vec3.cross orientation up

                basisMatrix =
                    Mat4.makeBasis perp up orientation

                changeVec =
                    Mat4.transform basisMatrix (Vec3.scale movementScale <| vec3 x 0 y)

                nextTargetPos =
                    Vec3.add me.eyeDirection changeVec

                nextMePos =
                    me.center |> Vec3.add changeVec

                nextModel =
                    { model | movementPadHeldMs = model.movementPadHeldMs + ms }
            in
            if isValidPos nextTargetPos && isValidPos nextMePos then
                { nextModel
                    | me =
                        { me
                            | center = nextMePos
                            , eyeDirection = nextTargetPos
                        }
                }

            else
                nextModel

        Nothing ->
            model


updateTarget : Float -> Model -> Model
updateTarget ms model =
    case model.targetPadOffset of
        Just relativePos ->
            let
                { me } =
                    model

                direction =
                    Vec2.direction relativePos model.targetPadOrigin

                { x, y } =
                    Vec2.toRecord <| direction

                orientation =
                    Vec3.direction me.eyeDirection me.center

                up =
                    vec3 0 1 0

                perp =
                    Vec3.cross orientation up

                basisMatrix =
                    Mat4.makeBasis perp up orientation

                changeVec =
                    Mat4.transform basisMatrix (Vec3.scale movementScale <| vec3 x y 0)

                nextTargetPos =
                    Vec3.add model.me.eyeDirection changeVec
            in
            if isValidPos nextTargetPos then
                { model
                    | me = { me | eyeDirection = nextTargetPos }
                    , targetPadHeldMs = model.targetPadHeldMs + ms
                }

            else
                model

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delta ms ->
            ( model |> updateY ms |> updateMovement ms |> updateTarget ms, Cmd.none )

        ViewportChanged { viewport } ->
            let
                { width, height } =
                    viewport

                shortDimension =
                    min width height
            in
            ( { model | viewSize = shortDimension - 20.0 }, Cmd.none )

        MovementPadDown newOrigin ->
            ( { model | movementPadOrigin = newOrigin, movementPadOffset = Just newOrigin }, Cmd.none )

        TargetPadDown newOrigin ->
            ( { model | targetPadOrigin = newOrigin, targetPadOffset = Just newOrigin }, Cmd.none )

        MovementPadMoved relativePos ->
            ( { model | movementPadOffset = Just relativePos }, Cmd.none )

        TargetPadMoved relativePos ->
            ( { model | targetPadOffset = Just relativePos }, Cmd.none )

        MovementPadUp ->
            let
                nextModel =
                    { model
                        | movementPadOffset = Nothing
                        , movementPadHeldMs = 0
                    }

                { me } =
                    model
            in
            if model.movementPadHeldMs < jumpThresholdMs && isOnFloor me.center then
                ( { nextModel
                    | me = { me | yVelocity = jumpVelocity }
                  }
                , Cmd.none
                )

            else
                ( nextModel, Cmd.none )

        TargetPadUp ->
            -- SHOOT if model.targetPadHeldMs < shootThresholdMs
            ( { model
                | targetPadOffset = Nothing
                , targetPadHeldMs = 0
              }
            , Cmd.none
            )


viewPad :
    Float
    -> (Vec2 -> Msg)
    -> (Vec2 -> Msg)
    -> Msg
    -> List (Html.Attribute Msg)
    -> Html Msg
viewPad viewSize onDown onMove onUp attrs =
    Html.div
        ([ Html.Attributes.style "touch-action" "none"
         , Html.Attributes.width (round viewSize)
         , Html.Attributes.style "height" "0"
         , Html.Attributes.style "padding-bottom" "50%"
         , Html.Attributes.style "flex" "1"
         , Html.Attributes.style "margin" "5px"
         , Pointer.onDown (\event -> onDown <| tupleToVec2 event.pointer.offsetPos)
         , Pointer.onMove (\event -> onMove <| tupleToVec2 event.pointer.offsetPos)
         , Pointer.onUp (always onUp)
         ]
            ++ attrs
        )
        []


tupleToVec2 : ( Float, Float ) -> Vec2
tupleToVec2 ( x, y ) =
    vec2 x (negate y)


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "height" "100%"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "touch-action" "none"
        ]
        [ Html.div
            [ Html.Attributes.style "flex" "2"
            ]
            [ WebGL.toHtml
                [ Html.Attributes.height (round model.viewSize)
                , Html.Attributes.width (round model.viewSize)
                , Html.Attributes.style "z-index" "1"
                , Html.Attributes.style "border" "1px solid black"
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    cubeMesh
                    (uniforms model)
                ]
            , Html.div
                [ Html.Attributes.style "height" ((String.fromInt <| round model.viewSize) ++ "px")
                , Html.Attributes.style "width" ((String.fromInt <| round model.viewSize) ++ "px")
                , Html.Attributes.style "z-index" "2"
                , Html.Attributes.style "background" "transparent"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0"
                ]
                [ Html.div
                    [ Html.Attributes.style "height" "20px"
                    , Html.Attributes.style "width" "20px"
                    , Html.Attributes.style "border" "2px solid black"
                    , Html.Attributes.style "border-radius" "100%"
                    ]
                    []
                ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            ]
            [ viewPad
                model.viewSize
                MovementPadDown
                MovementPadMoved
                MovementPadUp
                [ Html.Attributes.style "background-color" "red" ]
            , viewPad
                model.viewSize
                TargetPadDown
                TargetPadMoved
                TargetPadUp
                [ Html.Attributes.style "background-color" "blue" ]
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = Browser.Events.onAnimationFrameDelta << always Delta
        , update = update
        }


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Model -> Uniforms
uniforms model =
    { perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt model.me.center model.me.eyeDirection (vec3 0 1 0)
    , shade = 0.8
    }


type alias Person =
    { center : Vec3
    , color : Vec3
    , eyeDirection : Vec3
    , yVelocity : Float
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cubeMesh : WebGL.Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
    [ face (vec3 115 210 22) rft rfb rbb rbt -- green
    , face (vec3 52 101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0) rft lft lbt rbt -- yellow
    , face (vec3 204 0 0) rfb lfb lbb rbb -- red
    , face (vec3 117 80 123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0) rbt rbb lbb lbt -- orange
    ]
        |> List.concat
        |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]



-- Shaders


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
