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
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Json.Decode
import Json.Encode
import Length
import Mass
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Me
import Physics.Body
import Physics.Body.Extra
import Physics.World
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
    , movementPadOrigin : Vec2
    , movementPadOffset : Maybe Vec2
    , movementPadHeldMs : Float
    , targetPadOrigin : Vec2
    , targetPadOffset : Maybe Vec2
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
    | MovementPadDown Vec2
    | MovementPadMoved Vec2
    | MovementPadUp
    | TargetPadDown Vec2
    | TargetPadMoved Vec2
    | TargetPadUp
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
      , movementPadOrigin = vec2 0 0
      , movementPadOffset = Nothing
      , movementPadHeldMs = 0
      , targetPadOrigin = vec2 0 0
      , targetPadOffset = Nothing
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

        MovementPadDown newOrigin ->
            ( { gameState | movementPadOrigin = newOrigin, movementPadOffset = Just newOrigin }, Cmd.none )

        TargetPadDown newOrigin ->
            ( { gameState | targetPadOrigin = newOrigin, targetPadOffset = Just newOrigin }, Cmd.none )

        MovementPadMoved relativePos ->
            ( { gameState | movementPadOffset = Just relativePos }, Cmd.none )

        TargetPadMoved relativePos ->
            ( { gameState | targetPadOffset = Just relativePos }, Cmd.none )

        MovementPadUp ->
            let
                nextModel =
                    { gameState
                        | movementPadOffset = Nothing
                        , movementPadHeldMs = 0
                    }
            in
            if gameState.movementPadHeldMs < jumpThresholdMs then
                ( { nextModel | shouldJumpNextFrame = True }, Cmd.none )

            else
                ( nextModel, Cmd.none )

        TargetPadUp ->
            let
                nextModel =
                    { gameState
                        | targetPadOffset = Nothing
                        , targetPadHeldMs = 0
                    }
            in
            if gameState.targetPadHeldMs < shootThresholdMs then
                ( { nextModel | shouldShootNextFrame = True }, Cmd.none )

            else
                ( nextModel, Cmd.none )

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


addBodies :
    List (Physics.Body.Body BodyData.Data)
    -> Physics.World.World BodyData.Data
    -> Physics.World.World BodyData.Data
addBodies bodies world =
    List.foldl Physics.World.add world bodies


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
    case gameState.movementPadOffset of
        Just relativePos ->
            let
                direction =
                    Vec2.direction relativePos gameState.movementPadOrigin

                { x, y } =
                    Vec2.toRecord <| direction

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
    case gameState.targetPadOffset of
        Just relativePos ->
            let
                direction =
                    Vec2.direction gameState.targetPadOrigin relativePos

                { x, y } =
                    Vec2.toRecord <| direction

                xRotationDeg =
                    y / 2

                zRotationDeg =
                    x / 2

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
    Angle.degrees 30


view :
    Physics.World.World BodyData.Data
    -> Maybe Texture.Textures
    -> List Server.Team.Team
    -> Float
    -> Float
    -> Float
    -> Html Msg
view world textures teams cameraXRotationAngle cameraZRotationAngle viewSize =
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
                , dimensions = ( Pixels.int <| round viewSize, Pixels.int <| round viewSize )
                , camera = camera
                , clipDepth = Length.meters 0.5
                , background = Scene3d.backgroundColor Color.lightBlue
                , entities = entities
                }
            , Html.div
                [ Html.Attributes.style "height" ((String.fromInt <| round viewSize) ++ "px")
                , Html.Attributes.style "width" ((String.fromInt <| round viewSize) ++ "px")
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
                viewSize
                MovementPadDown
                MovementPadMoved
                MovementPadUp
                [ Html.Attributes.style "background-color" "red" ]
            , viewPad
                viewSize
                TargetPadDown
                TargetPadMoved
                TargetPadUp
                [ Html.Attributes.style "background-color" "blue" ]
            ]
        ]


viewPad :
    Float
    -> (Vec2 -> msg)
    -> (Vec2 -> msg)
    -> msg
    -> List (Html.Attribute msg)
    -> Html msg
viewPad viewSize onDown onMove onUp attrs =
    Html.div
        ([ Html.Attributes.style "touch-action" "none"
         , Html.Attributes.style "user-select" "none"
         , Html.Attributes.style "-webkit-user-select" "none"
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


port requestJump : String -> Cmd msg


port requestMove : { playerId : String, x : Float, y : Float } -> Cmd msg


port requestRotate : { playerId : String, angle : Float } -> Cmd msg


port requestShoot : { playerId : String, position : { x : Float, y : Float, z : Float }, linvel : { x : Float, y : Float, z : Float } } -> Cmd msg
