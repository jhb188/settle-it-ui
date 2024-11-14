port module Game.Playing exposing (..)

import Acceleration
import Angle
import Axis3d
import BodyData exposing (Class(..), Data, Dimensions(..))
import Browser.Dom
import Camera3d
import Color
import Common exposing (..)
import Config
import Direction3d
import Duration
import Force
import Frame3d
import Game.Playing.Joystick as Joystick exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Json.Decode
import Json.Encode
import Length
import Mass
import Me
import Physics.Body
import Physics.Body.Extra
import Physics.World
import Physics.World.Extra exposing (addBodies)
import Pixels
import Point3d
import Quantity
import Scene3d
import Server.State
import Server.Team
import Task
import Texture
import Vector3d
import WebGL
import WebGL.Texture


type alias Model =
    { playerId : PlayerId
    , viewSize : Float
    , arena : Physics.World.World BodyData.Data
    , movementJoystickState : Joystick.Model
    , movementPadHeldMs : Float
    , targetJoystickState : Joystick.Model
    , targetPadHeldMs : Float
    , world : Physics.World.World Data
    , lastUpdated : Int
    , newBodiesUpdate : Maybe Json.Encode.Value
    , msSinceLastServerUpdateApplied : Float
    , cameraXRotationAngle : Float
    , cameraZRotationAngle : Float
    , shouldJumpNextFrame : Bool
    , shouldShootNextFrame : Bool
    , teams : List Server.Team.Team
    , textures : Maybe Texture.Textures
    }


type Msg
    = Delta Float
    | ViewportChanged Browser.Dom.Viewport
    | MovementPadUpdated JoystickMsg
    | TargetPadUpdated JoystickMsg
    | BodiesUpdated Json.Encode.Value
    | TexturesLoaded (Result WebGL.Texture.Error Texture.Textures)


init : PlayerId -> Server.State.State -> ( Model, Cmd Msg )
init playerId { bodies, lastUpdated, teams } =
    let
        arenaObstacles =
            List.filter (Physics.Body.data >> .class >> (==) Obstacle) bodies

        arena =
            addBodies arenaObstacles initWorld

        world =
            addBodies bodies initWorld

        initialCameraZRotationAngle =
            case Me.getInWorld world of
                Just me ->
                    let
                        myLookDirection =
                            Frame3d.yDirection <| Physics.Body.frame me

                        myRotationAngle =
                            Direction3d.angleFrom (Frame3d.yDirection Frame3d.atOrigin) myLookDirection |> Angle.inDegrees
                    in
                    if Direction3d.xComponent myLookDirection > 0 then
                        360.0 - myRotationAngle

                    else
                        myRotationAngle

                Nothing ->
                    0
    in
    ( { playerId = playerId
      , viewSize = 0
      , arena = arena
      , movementJoystickState = Joystick.init
      , movementPadHeldMs = 0
      , targetJoystickState = Joystick.init
      , targetPadHeldMs = 0
      , world = world
      , lastUpdated = lastUpdated
      , newBodiesUpdate = Nothing
      , msSinceLastServerUpdateApplied = 0.0
      , cameraXRotationAngle = 0.0
      , cameraZRotationAngle = initialCameraZRotationAngle
      , shouldJumpNextFrame = False
      , shouldShootNextFrame = False
      , teams = teams
      , textures = Nothing
      }
    , Task.perform ViewportChanged Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg gameState =
    case msg of
        Delta ms ->
            let
                nextModel =
                    case Maybe.map (Json.Decode.decodeValue (Server.State.bodiesStateDecoder gameState.playerId)) gameState.newBodiesUpdate of
                        Just (Ok { bodies, lastUpdated }) ->
                            let
                                isNewServerState =
                                    lastUpdated > gameState.lastUpdated
                            in
                            if isNewServerState then
                                let
                                    defaultNextWorld =
                                        gameState.arena
                                            |> addBodies bodies

                                    nextWorld =
                                        case ( Me.getInWorld gameState.world, Me.getInBodies bodies ) of
                                            ( Just oldMe, Just newMe ) ->
                                                let
                                                    nextMe =
                                                        Physics.Body.Extra.updateServerAuthoritativeData oldMe newMe
                                                in
                                                Me.updateInWorld (always nextMe) defaultNextWorld

                                            _ ->
                                                defaultNextWorld

                                    clientServerDiscrepancyMs =
                                        (toFloat gameState.lastUpdated + gameState.msSinceLastServerUpdateApplied + ms) - toFloat lastUpdated
                                in
                                { gameState
                                    | world = simulatePhysics clientServerDiscrepancyMs nextWorld
                                    , lastUpdated = lastUpdated
                                    , newBodiesUpdate = Nothing
                                    , msSinceLastServerUpdateApplied = 0
                                }

                            else
                                getNextSimulatedPlayingState ms gameState

                        _ ->
                            getNextSimulatedPlayingState ms gameState

                ( nextModelActionsApplied, cmd ) =
                    applyActions (Me.getInWorld gameState.world) nextModel ms
            in
            ( nextModelActionsApplied, cmd )

        BodiesUpdated bodiesUpdate ->
            ( { gameState | newBodiesUpdate = Just bodiesUpdate }, Cmd.none )

        ViewportChanged { viewport } ->
            let
                { width, height } =
                    viewport

                shortDimension =
                    min width height
            in
            ( { gameState | viewSize = shortDimension - 20.0 }, Task.attempt TexturesLoaded Texture.init )

        MovementPadUpdated joystickUpdate ->
            let
                nextGameState =
                    { gameState
                        | movementJoystickState =
                            Joystick.update
                                gameState.movementJoystickState
                                joystickUpdate
                    }
            in
            case joystickUpdate of
                JoystickUp ->
                    let
                        nextModel =
                            { nextGameState | movementPadHeldMs = 0 }
                    in
                    if gameState.movementPadHeldMs < jumpThresholdMs then
                        ( { nextModel | shouldJumpNextFrame = True }, Cmd.none )

                    else
                        ( nextModel, Cmd.none )

                _ ->
                    ( nextGameState, Cmd.none )

        TargetPadUpdated joystickUpdate ->
            let
                nextGameState =
                    { gameState
                        | targetJoystickState =
                            Joystick.update
                                gameState.targetJoystickState
                                joystickUpdate
                    }
            in
            case joystickUpdate of
                JoystickUp ->
                    let
                        nextModel =
                            { nextGameState | targetPadHeldMs = 0 }
                    in
                    if gameState.targetPadHeldMs < shootThresholdMs then
                        ( { nextModel | shouldShootNextFrame = True }, Cmd.none )

                    else
                        ( nextModel, Cmd.none )

                _ ->
                    ( nextGameState, Cmd.none )

        TexturesLoaded result ->
            case result of
                Ok textures ->
                    ( { gameState | textures = Just textures }, Cmd.none )

                Err _ ->
                    ( gameState, Cmd.none )


jumpThresholdMs : Float
jumpThresholdMs =
    300.0


shootThresholdMs : Float
shootThresholdMs =
    300.0


getNextSimulatedPlayingState : Float -> Model -> Model
getNextSimulatedPlayingState ms gameState =
    { gameState
        | world = simulatePhysics ms gameState.world
        , newBodiesUpdate = Nothing
        , msSinceLastServerUpdateApplied = gameState.msSinceLastServerUpdateApplied + ms
    }


simulatePhysics : Float -> Physics.World.World Data -> Physics.World.World Data
simulatePhysics ms world =
    if Config.useClientPhysics && ms > 0 then
        Physics.World.simulate (Duration.milliseconds ms) world

    else
        world


initWorld : Physics.World.World Data
initWorld =
    Physics.World.empty
        |> Physics.World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ


amIAlive : Model -> Bool
amIAlive { world } =
    case Me.getInWorld world of
        Nothing ->
            False

        Just me ->
            isAlive me


applyActions : Maybe (Physics.Body.Body BodyData.Data) -> Model -> Float -> ( Model, Cmd Msg )
applyActions previousMe gameState ms =
    let
        playerIsAlive =
            amIAlive gameState

        applyIf cond f gs =
            if cond then
                f gs

            else
                ( gs, Cmd.none )

        ( gameStateMovementApplied, movementCmds ) =
            applyIf playerIsAlive (updateMovement ms previousMe) gameState

        ( gameStateRotationApplied, rotationCmds ) =
            updateTarget ms gameStateMovementApplied

        ( gameStateJumpApplied, jumpCmd ) =
            applyIf (gameState.shouldJumpNextFrame && playerIsAlive) applyJump gameStateRotationApplied

        ( gameStateShootApplied, shootCmd ) =
            applyIf (gameState.shouldShootNextFrame && playerIsAlive) applyShoot gameStateJumpApplied
    in
    ( { gameStateShootApplied | shouldJumpNextFrame = False, shouldShootNextFrame = False }, Cmd.batch [ movementCmds, rotationCmds, jumpCmd, shootCmd ] )


applyJump : Model -> ( Model, Cmd Msg )
applyJump gameState =
    ( gameState
    , requestJump gameState.playerId
    )


bulletMass : Mass.Mass
bulletMass =
    Mass.grams 50


bulletImpulse : Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
bulletImpulse =
    Force.newtons 200 |> Quantity.times (Duration.milliseconds 16.667)


applyShoot : Model -> ( Model, Cmd Msg )
applyShoot gameState =
    let
        projectileOrientation =
            Me.getLookAxis gameState.world gameState.cameraXRotationAngle gameState.cameraZRotationAngle

        projectileStartPoint =
            Point3d.along projectileOrientation (Length.centimeters 30)

        projectile =
            Physics.Body.particle { class = Bullet, mesh = WebGL.triangles [], hp = 0 }
                |> Physics.Body.withBehavior (Physics.Body.dynamic bulletMass)
                |> Physics.Body.moveTo projectileStartPoint
                |> Physics.Body.applyImpulse
                    bulletImpulse
                    (Axis3d.direction projectileOrientation)
                    projectileStartPoint
    in
    ( gameState
    , requestShoot
        { playerId = gameState.playerId
        , position = Point3d.toRecord Length.inMeters projectileStartPoint
        , linvel = getLinvel projectile
        }
    )


getLinvel : Physics.Body.Body data -> { x : Float, y : Float, z : Float }
getLinvel body =
    body
        |> Physics.Body.velocity
        |> Vector3d.for (Duration.seconds 1)
        |> Vector3d.toRecord Length.inMeters


isAlive : Physics.Body.Body Data -> Bool
isAlive body =
    let
        data =
            Physics.Body.data body
    in
    data.hp > 0


updateMovement : Float -> Maybe (Physics.Body.Body BodyData.Data) -> Model -> ( Model, Cmd Msg )
updateMovement ms maybeMe gameState =
    case Joystick.getDirection gameState.movementJoystickState of
        Just { x, y } ->
            let
                speed =
                    0.1

                xMovement =
                    x * speed

                yMovement =
                    y * speed

                coordsAreInvalid =
                    isNaN x || isNaN y

                ( gameStateWithUpdatedWorld, cmds ) =
                    case ( maybeMe, coordsAreInvalid ) of
                        ( Just me, False ) ->
                            let
                                myFrame =
                                    Physics.Body.frame me

                                maybeNextZ =
                                    Me.getInWorld gameState.world |> Maybe.map (Physics.Body.originPoint >> Point3d.zCoordinate >> Length.inMeters)

                                nextOrigin =
                                    myFrame
                                        |> Frame3d.translateAlongOwn Frame3d.yAxis (Length.meters yMovement)
                                        |> Frame3d.translateAlongOwn Frame3d.xAxis (Length.meters xMovement)
                                        |> Frame3d.originPoint

                                bodyFrameMoved =
                                    case maybeNextZ of
                                        Just z ->
                                            let
                                                nextOriginCoords =
                                                    Point3d.toMeters nextOrigin
                                            in
                                            Frame3d.moveTo (Point3d.fromMeters { x = nextOriginCoords.x, y = nextOriginCoords.y, z = z }) myFrame

                                        Nothing ->
                                            Frame3d.moveTo nextOrigin myFrame

                                nextOriginPoint =
                                    Frame3d.placeIn Frame3d.atOrigin bodyFrameMoved |> Frame3d.originPoint

                                nextWorld =
                                    Me.updateInWorld (Physics.Body.moveTo nextOriginPoint) gameState.world
                            in
                            ( { gameState
                                | world = nextWorld
                              }
                            , requestMove
                                { playerId = gameState.playerId
                                , x = Point3d.xCoordinate nextOriginPoint |> Length.inMeters
                                , y = Point3d.yCoordinate nextOriginPoint |> Length.inMeters
                                }
                            )

                        _ ->
                            ( gameState, Cmd.none )
            in
            ( { gameStateWithUpdatedWorld | movementPadHeldMs = gameState.movementPadHeldMs + ms }, cmds )

        Nothing ->
            ( gameState, Cmd.none )


updateTarget : Float -> Model -> ( Model, Cmd Msg )
updateTarget ms gameState =
    case Joystick.getDirection gameState.targetJoystickState of
        Just { x, y } ->
            let
                xRotationDeg =
                    -y

                zRotationDeg =
                    -x

                coordsAreInvalid =
                    isNaN x || isNaN y
            in
            if coordsAreInvalid then
                ( gameState, Cmd.none )

            else
                let
                    newZRotationAngle =
                        gameState.cameraZRotationAngle + zRotationDeg

                    newZRotationAngleNormalized =
                        if newZRotationAngle > 360.0 then
                            360.0 - newZRotationAngle

                        else
                            newZRotationAngle
                in
                ( { gameState
                    | cameraXRotationAngle = gameState.cameraXRotationAngle - xRotationDeg
                    , cameraZRotationAngle = newZRotationAngleNormalized
                    , targetPadHeldMs = gameState.targetPadHeldMs + ms
                  }
                , requestRotate { playerId = gameState.playerId, angle = newZRotationAngleNormalized }
                )

        Nothing ->
            ( gameState, Cmd.none )


fovAngle : Angle.Angle
fovAngle =
    Angle.degrees 60


view : Model -> Html Msg
view playingState =
    Html.div
        [ Html.Attributes.style "height" "100%"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "touch-action" "none"
        ]
        [ Html.Lazy.lazy6 viewGameScene
            playingState.world
            playingState.textures
            playingState.teams
            playingState.cameraXRotationAngle
            playingState.cameraZRotationAngle
            playingState.viewSize
        , viewControlPad
            playingState
        ]


viewGameScene :
    Physics.World.World Data
    -> Maybe Texture.Textures
    -> List Server.Team.Team
    -> Float
    -> Float
    -> Float
    -> Html Msg
viewGameScene world textures teams cameraXRotationAngle cameraZRotationAngle viewSize =
    let
        viewpoint =
            Me.getViewpoint
                world
                cameraXRotationAngle
                cameraZRotationAngle

        camera =
            Camera3d.perspective { viewpoint = viewpoint, verticalFieldOfView = fovAngle }

        entities =
            world
                |> Physics.World.bodies
                |> List.filterMap (Physics.Body.Extra.toSceneEntity textures teams viewpoint)
    in
    Html.div
        [ Html.Attributes.class "no-zoom"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection = Direction3d.yz (Angle.degrees -120)
            , shadows = True
            , dimensions = ( Pixels.int <| round viewSize, Pixels.int <| round viewSize )
            , camera = camera
            , clipDepth = Length.meters 0.5
            , background = Scene3d.backgroundColor Color.lightBlue
            , entities = entities
            }
        ]


viewControlPad : Model -> Html Msg
viewControlPad model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "flex" "1"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.class "no-zoom"
        ]
        [ Joystick.view
            model.viewSize
            MovementPadUpdated
            model.movementJoystickState
        , Joystick.view
            model.viewSize
            TargetPadUpdated
            model.targetJoystickState
        ]


port requestJump : String -> Cmd msg


port requestMove : { playerId : String, x : Float, y : Float } -> Cmd msg


port requestRotate : { playerId : String, angle : Float } -> Cmd msg


port requestShoot : { playerId : String, position : { x : Float, y : Float, z : Float }, linvel : { x : Float, y : Float, z : Float } } -> Cmd msg
