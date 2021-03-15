port module Main exposing (main)

import Acceleration
import Angle
import Axis3d
import BodyData exposing (Class(..), Data)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Cylinder3d
import Direction3d
import Duration
import Force
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Json.Decode
import Json.Encode
import Length
import List
import List.Extra
import Mass
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Physics.Body
import Physics.Coordinates
import Physics.World
import Pixels
import Point2d
import Point3d
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Material
import Server.State
import Sphere3d
import String
import Task
import Vector3d
import Viewpoint3d
import WebGL



-- globally enable/disable client physics simulations for dev purposes


useClientPhysics : Bool
useClientPhysics =
    True


type alias Flags =
    { playerId : String
    }


type alias GameState =
    { playerId : String
    , viewSize : Float
    , movementPadOrigin : Vec2
    , movementPadOffset : Maybe Vec2
    , movementPadHeldMs : Float
    , targetPadOrigin : Vec2
    , targetPadOffset : Maybe Vec2
    , targetPadHeldMs : Float
    , world : Physics.World.World Data
    , focalPoint : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    , camera : Camera3d.Camera3d Length.Meters Physics.Coordinates.WorldCoordinates
    , lastUpdated : Int
    , newServerState : Maybe Json.Encode.Value
    , msSinceLastServerUpdateApplied : Int
    }


type alias PlayerId =
    String


type Model
    = Waiting PlayerId
    | Playing GameState


type Msg
    = Delta Float
    | ViewportChanged Browser.Dom.Viewport
    | MovementPadDown Vec2
    | MovementPadMoved Vec2
    | MovementPadUp
    | TargetPadDown Vec2
    | TargetPadMoved Vec2
    | TargetPadUp
    | GameUpdated Json.Encode.Value


fovAngle : Angle.Angle
fovAngle =
    Angle.degrees 30


floor : Physics.Body.Body BodyData.Data
floor =
    Physics.Body.plane { mesh = WebGL.triangles [], class = Floor }


initWorld : Physics.World.World Data
initWorld =
    Physics.World.empty
        |> Physics.World.withGravity
            (Acceleration.gees 1)
            Direction3d.negativeZ
        |> Physics.World.add floor


updateClass :
    Class
    -> (Physics.Body.Body Data -> Physics.Body.Body Data)
    -> Physics.World.World Data
    -> Physics.World.World Data
updateClass class transformer world =
    let
        f body =
            let
                data =
                    Physics.Body.data body
            in
            if data.class == class then
                transformer body

            else
                body
    in
    Physics.World.update f world


updateMe : (Physics.Body.Body Data -> Physics.Body.Body Data) -> Physics.World.World Data -> Physics.World.World Data
updateMe =
    updateClass Me


hasClass : Class -> Physics.Body.Body Data -> Bool
hasClass class body =
    let
        data =
            Physics.Body.data body
    in
    data.class == class


getMe : Physics.World.World Data -> Maybe (Physics.Body.Body Data)
getMe =
    Physics.World.bodies >> List.Extra.find (hasClass Me)


getEyePoint : Physics.Body.Body Data -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
getEyePoint me =
    Physics.Body.originPoint me
        |> Point3d.translateBy (Vector3d.meters 0 0 0.5)


getLookAxis : GameState -> Axis3d.Axis3d Length.Meters Physics.Coordinates.WorldCoordinates
getLookAxis { viewSize, camera } =
    let
        screenFocusedPoint =
            viewSize / 2
    in
    Camera3d.ray
        camera
        (Rectangle2d.from
            Point2d.origin
            (Point2d.pixels viewSize viewSize)
        )
        (Point2d.pixels screenFocusedPoint screenFocusedPoint)


atRest : Physics.Body.Body Data -> Bool
atRest body =
    let
        { x, y, z } =
            Vector3d.unwrap <| Physics.Body.velocity body
    in
    ( round x, round y, round z ) == ( 0, 0, 0 )


removeStaleBullets : Physics.World.World Data -> Physics.World.World Data
removeStaleBullets =
    Physics.World.keepIf
        (\body ->
            let
                data =
                    Physics.Body.data body
            in
            case data.class of
                Bullet ->
                    not <| atRest body

                _ ->
                    True
        )


jumpImpulse =
    Force.newtons 2400 |> Quantity.times (Duration.seconds 0.15)


jump : Physics.Body.Body Data -> Physics.Body.Body Data
jump body =
    let
        hitPoint =
            Physics.Body.originPoint body
    in
    Physics.Body.applyImpulse jumpImpulse Direction3d.positiveZ hitPoint body


addBodies :
    List (Physics.Body.Body BodyData.Data)
    -> Physics.World.World BodyData.Data
    -> Physics.World.World BodyData.Data
addBodies bodies world =
    List.foldl Physics.World.add world bodies


init : Flags -> ( Model, Cmd Msg )
init { playerId } =
    ( Waiting playerId, joinGame ( "aaaa", playerId ) )


initPlaying : String -> Server.State.State -> ( Model, Cmd Msg )
initPlaying playerId { bodies, lastUpdated } =
    let
        world =
            addBodies bodies initWorld

        ( focalPoint, eyePoint ) =
            case getMe world of
                Just me ->
                    ( getInitialFocalPoint me, getEyePoint me )

                Nothing ->
                    ( Point3d.origin, Point3d.origin )

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint = focalPoint
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = fovAngle
                }
    in
    ( Playing
        { playerId = playerId
        , viewSize = 0
        , movementPadOrigin = vec2 0 0
        , movementPadOffset = Nothing
        , movementPadHeldMs = 0
        , targetPadOrigin = vec2 0 0
        , targetPadOffset = Nothing
        , targetPadHeldMs = 0
        , world = world
        , focalPoint = focalPoint
        , camera = camera
        , lastUpdated = lastUpdated
        , newServerState = Nothing
        , msSinceLastServerUpdateApplied = 0
        }
    , Task.perform ViewportChanged Browser.Dom.getViewport
    )


getInitialFocalPoint : Physics.Body.Body BodyData.Data -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
getInitialFocalPoint me =
    let
        myFrame =
            Physics.Body.frame me

        eyePoint =
            getEyePoint me

        lookAxis =
            myFrame |> Frame3d.yAxis |> Axis3d.moveTo eyePoint
    in
    Point3d.along lookAxis (Length.meters 1)


jumpThresholdMs : Float
jumpThresholdMs =
    300.0


shootThresholdMs : Float
shootThresholdMs =
    300.0


updateMovement : Float -> GameState -> ( GameState, Cmd Msg )
updateMovement ms model =
    case model.movementPadOffset of
        Just relativePos ->
            let
                direction =
                    Vec2.direction relativePos model.movementPadOrigin

                { x, y } =
                    Vec2.toRecord <| direction

                xMovement =
                    x / 10

                yMovement =
                    y / 10

                coordsAreInvalid =
                    isNaN x || isNaN y

                ( modelWithUpdatedWorld, cmds ) =
                    case ( getMe model.world, coordsAreInvalid ) of
                        ( Just me, False ) ->
                            let
                                myFrame =
                                    Physics.Body.frame me

                                bodyFrameMoved =
                                    myFrame
                                        |> Frame3d.translateAlongOwn Frame3d.yAxis (Length.meters yMovement)
                                        |> Frame3d.translateAlongOwn Frame3d.xAxis (Length.meters xMovement)

                                nextOriginPoint =
                                    Frame3d.placeIn Frame3d.atOrigin bodyFrameMoved |> Frame3d.originPoint
                            in
                            ( { model | world = updateMe (Physics.Body.moveTo nextOriginPoint) model.world }
                            , requestMove
                                { playerId = model.playerId
                                , x = Point3d.xCoordinate nextOriginPoint |> Length.inMeters
                                , y = Point3d.yCoordinate nextOriginPoint |> Length.inMeters
                                }
                            )

                        _ ->
                            ( model, Cmd.none )

                nextModel =
                    { modelWithUpdatedWorld | movementPadHeldMs = model.movementPadHeldMs + ms }
            in
            ( nextModel, cmds )

        Nothing ->
            ( model, Cmd.none )


updateTarget : Float -> GameState -> GameState
updateTarget ms model =
    case model.targetPadOffset of
        Just relativePos ->
            let
                direction =
                    Vec2.direction model.targetPadOrigin relativePos

                { x, y } =
                    Vec2.toRecord <| direction

                yRotationDeg =
                    y / 2

                zRotationDeg =
                    x / 2

                currentCameraAxis =
                    getLookAxis model

                currentCameraFrame =
                    Frame3d.fromXAxis currentCameraAxis

                nextCameraFrame =
                    currentCameraFrame
                        |> Frame3d.rotateAroundOwn Frame3d.yAxis (Angle.degrees yRotationDeg)

                nextWorld =
                    updateMe (Physics.Body.rotateAround Axis3d.z (Angle.degrees -zRotationDeg)) model.world

                nextCameraAxis =
                    Frame3d.xAxis nextCameraFrame

                nextFocalPoint =
                    Point3d.along nextCameraAxis (Length.meters 1)

                nextCamera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { eyePoint = Axis3d.originPoint nextCameraAxis
                                , focalPoint = nextFocalPoint
                                , upDirection = Direction3d.positiveZ
                                }
                        , verticalFieldOfView = fovAngle
                        }
            in
            if not (isNaN x || isNaN y) then
                { model
                    | camera = nextCamera
                    , targetPadHeldMs = model.targetPadHeldMs + ms
                    , world = nextWorld
                }

            else
                model

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Waiting playerId ->
            case msg of
                GameUpdated gameUpdateData ->
                    case Json.Decode.decodeValue (Server.State.decoder playerId) gameUpdateData of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok serverState ->
                            initPlaying playerId serverState

                _ ->
                    ( model, Cmd.none )

        Playing gameState ->
            let
                ( nextGameState, cmds ) =
                    updateGameState msg gameState
            in
            ( Playing nextGameState, cmds )


simulatePhysics : Physics.World.World Data -> Float -> Physics.World.World Data
simulatePhysics world ms =
    if useClientPhysics then
        Physics.World.simulate (Duration.milliseconds ms) world
            |> removeStaleBullets

    else
        world


getNextSimulatedGameState : Float -> GameState -> GameState
getNextSimulatedGameState ms gameState =
    let
        world =
            simulatePhysics gameState.world ms

        nextCamera =
            case getMe world of
                Nothing ->
                    gameState.camera

                Just me ->
                    let
                        nextEyePoint =
                            getEyePoint me

                        currentCameraAxis =
                            getLookAxis gameState

                        nextCameraAxis =
                            Axis3d.moveTo nextEyePoint currentCameraAxis

                        nextFocalPoint =
                            Point3d.along nextCameraAxis (Length.meters 1)
                    in
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { eyePoint = nextEyePoint
                                , focalPoint = nextFocalPoint
                                , upDirection = Direction3d.positiveZ
                                }
                        , verticalFieldOfView = fovAngle
                        }
    in
    { gameState
        | camera = nextCamera
        , world = world
        , newServerState = Nothing
        , msSinceLastServerUpdateApplied = gameState.msSinceLastServerUpdateApplied + ceiling ms
    }


serverUpdateErrorThresholdMs : Int
serverUpdateErrorThresholdMs =
    500


updateGameState : Msg -> GameState -> ( GameState, Cmd Msg )
updateGameState msg model =
    case msg of
        Delta ms ->
            let
                nextModel =
                    case model.newServerState of
                        Just gameUpdateData ->
                            case Json.Decode.decodeValue (Server.State.decoder model.playerId) gameUpdateData of
                                Err _ ->
                                    getNextSimulatedGameState ms model

                                Ok { bodies, lastUpdated } ->
                                    if
                                        (lastUpdated > model.lastUpdated)
                                            && (abs (lastUpdated - (model.lastUpdated + model.msSinceLastServerUpdateApplied)) < serverUpdateErrorThresholdMs)
                                    then
                                        let
                                            nextWorld =
                                                addBodies bodies initWorld

                                            nextCamera =
                                                case getMe nextWorld of
                                                    Nothing ->
                                                        model.camera

                                                    Just me ->
                                                        let
                                                            nextEyePoint =
                                                                getEyePoint me

                                                            currentCameraAxis =
                                                                getLookAxis model

                                                            nextCameraAxis =
                                                                Axis3d.moveTo nextEyePoint currentCameraAxis

                                                            nextFocalPoint =
                                                                Point3d.along nextCameraAxis (Length.meters 1)
                                                        in
                                                        Camera3d.perspective
                                                            { viewpoint =
                                                                Viewpoint3d.lookAt
                                                                    { eyePoint = nextEyePoint
                                                                    , focalPoint = nextFocalPoint
                                                                    , upDirection = Direction3d.positiveZ
                                                                    }
                                                            , verticalFieldOfView = fovAngle
                                                            }
                                        in
                                        { model
                                            | world = nextWorld
                                            , camera = nextCamera
                                            , lastUpdated = lastUpdated
                                            , newServerState = Nothing
                                            , msSinceLastServerUpdateApplied = 0
                                        }

                                    else
                                        getNextSimulatedGameState ms model

                        Nothing ->
                            getNextSimulatedGameState ms model

                ( nextModelMovementApplied, movementCmds ) =
                    updateMovement ms nextModel
            in
            ( nextModelMovementApplied |> updateTarget ms, movementCmds )

        GameUpdated gameUpdateData ->
            ( { model | newServerState = Just gameUpdateData }, Cmd.none )

        ViewportChanged { viewport } ->
            let
                { width, height } =
                    viewport

                shortDimension =
                    min width height

                nextModel =
                    { model | viewSize = shortDimension - 20.0 }

                nextCamera =
                    case getMe model.world of
                        Nothing ->
                            model.camera

                        Just me ->
                            let
                                eyePoint =
                                    getEyePoint me
                            in
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.lookAt
                                        { eyePoint = eyePoint
                                        , focalPoint = getInitialFocalPoint me
                                        , upDirection = Direction3d.positiveZ
                                        }
                                , verticalFieldOfView = fovAngle
                                }
            in
            ( { nextModel | camera = nextCamera }, Cmd.none )

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

                { world } =
                    model
            in
            if model.movementPadHeldMs < jumpThresholdMs then
                ( { nextModel
                    | world = updateMe jump world
                  }
                , requestJump model.playerId
                )

            else
                ( nextModel, Cmd.none )

        TargetPadUp ->
            let
                nextModel =
                    { model
                        | targetPadOffset = Nothing
                        , targetPadHeldMs = 0
                    }
            in
            if model.targetPadHeldMs < shootThresholdMs then
                let
                    projectileOrientation =
                        getLookAxis model

                    projectileStartPoint =
                        Axis3d.originPoint projectileOrientation

                    projectile =
                        Physics.Body.particle { class = Bullet, mesh = WebGL.triangles [] }
                            |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.grams 50))
                            |> Physics.Body.moveTo projectileStartPoint
                            |> Physics.Body.applyForce
                                (Force.newtons 250)
                                (Axis3d.direction projectileOrientation)
                                projectileStartPoint

                    nextWorld =
                        Physics.World.add projectile model.world
                in
                ( { nextModel | world = nextWorld }
                , Cmd.none
                )

            else
                ( nextModel, Cmd.none )


viewPad :
    Float
    -> (Vec2 -> Msg)
    -> (Vec2 -> Msg)
    -> Msg
    -> List (Html.Attribute Msg)
    -> Html Msg
viewPad viewSize onDown onMove onUp attrs =
    Html.div
        ([ Html.Attributes.style "touch-action" "manipulation"
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
    case model of
        Playing gameState ->
            let
                entities =
                    gameState.world
                        |> Physics.World.bodies
                        |> List.filterMap
                            (\body ->
                                let
                                    bodyFrame =
                                        Physics.Body.frame body

                                    bodyClass =
                                        .class <| Physics.Body.data body
                                in
                                case bodyClass of
                                    Test ->
                                        Just <|
                                            (Scene3d.sphereWithShadow
                                                (Scene3d.Material.nonmetal
                                                    { baseColor = Color.red
                                                    , roughness = 0.4
                                                    }
                                                )
                                                (Sphere3d.atOrigin (Length.meters 0.5))
                                                |> Scene3d.placeIn bodyFrame
                                            )

                                    Floor ->
                                        Just <|
                                            Scene3d.quad (Scene3d.Material.matte Color.lightCharcoal)
                                                (Point3d.meters -100 -100 0)
                                                (Point3d.meters -100 100 0)
                                                (Point3d.meters 100 100 0)
                                                (Point3d.meters 100 -100 0)

                                    Bullet ->
                                        Just <|
                                            (Scene3d.sphere
                                                (Scene3d.Material.matte Color.blue)
                                                (Sphere3d.atOrigin (Length.centimeters 5))
                                                |> Scene3d.placeIn bodyFrame
                                            )

                                    NPC ->
                                        Just <|
                                            (Scene3d.cylinderWithShadow
                                                (Scene3d.Material.matte Color.green)
                                                (Cylinder3d.centeredOn
                                                    Point3d.origin
                                                    Direction3d.z
                                                    { radius = Length.meters 0.2
                                                    , length = Length.meters 2.0
                                                    }
                                                )
                                                |> Scene3d.placeIn bodyFrame
                                            )

                                    _ ->
                                        Nothing
                            )
            in
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
                    , Html.Attributes.style "z-index" "1"
                    , Html.Attributes.style "border" "1px solid black"
                    ]
                    [ Scene3d.sunny
                        { upDirection = Direction3d.positiveZ
                        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
                        , shadows = True
                        , dimensions = ( Pixels.int <| round gameState.viewSize, Pixels.int <| round gameState.viewSize )
                        , camera = gameState.camera
                        , clipDepth = Length.meters 0.5
                        , background = Scene3d.transparentBackground
                        , entities = entities
                        }
                    , Html.div
                        [ Html.Attributes.style "height" ((String.fromInt <| round gameState.viewSize) ++ "px")
                        , Html.Attributes.style "width" ((String.fromInt <| round gameState.viewSize) ++ "px")
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
                        gameState.viewSize
                        MovementPadDown
                        MovementPadMoved
                        MovementPadUp
                        [ Html.Attributes.style "background-color" "red" ]
                    , viewPad
                        gameState.viewSize
                        TargetPadDown
                        TargetPadMoved
                        TargetPadUp
                        [ Html.Attributes.style "background-color" "blue" ]
                    ]
                ]

        _ ->
            Html.div [] [ Html.text "Loading..." ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Delta
                    , gameUpdated GameUpdated
                    ]
        , update = update
        }


port joinGame : ( String, String ) -> Cmd msg


port requestJump : String -> Cmd msg


port requestMove : { playerId : String, x : Float, y : Float } -> Cmd msg


port gameUpdated : (Json.Encode.Value -> msg) -> Sub msg
