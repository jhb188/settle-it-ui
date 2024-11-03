port module Main exposing (main)

import Acceleration
import Angle
import Axis3d
import Block3d
import BodyData exposing (Class(..), Data, Dimensions(..))
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
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy
import Json.Decode
import Json.Encode
import Length
import List
import List.Extra
import Mass
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Mesh.Player
import Physics.Body
import Physics.Body.Extra exposing (getEyePoint)
import Physics.Coordinates
import Physics.World
import Physics.World.Extra exposing (getLookAxis, getMe, getMeInBodies, updateMe)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Server.Player
import Server.State
import Server.Team
import Sphere3d
import String
import Task
import Vector3d
import Viewpoint3d
import WebGL


useClientPhysics : Bool
useClientPhysics =
    False


type alias Flags =
    { playerId : PlayerId
    }


type alias PreLobbyState =
    { gameCode : String
    , topic : String
    , playerId : PlayerId
    }


type alias LobbyState =
    { playerId : PlayerId
    , teams : List Server.Team.Team
    , players : List Server.Player.Player
    , topic : String
    , gameId : String
    , teamNameInput : String
    , isEditingPlayerName : Bool
    , playerNameInput : String
    }


type alias PlayingState =
    { playerId : String
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
    }


type alias PostGameState =
    { playerId : String
    , teams : List Server.Team.Team
    , players : List ( Server.Player.Player, PlayerStats )
    , topic : String
    }


type alias PlayerStats =
    { hp : Int }


type alias PlayerId =
    String


type alias Model =
    ( PlayerId, Game )


type Game
    = PreLobby PreLobbyState
    | Lobby LobbyState
    | Playing PlayingState
    | PostGame PostGameState


type PreLobbyMsg
    = GameCodeInputUpdated String
    | TopicUpdated String
    | JoinGame
    | CreateGame


type LobbyMsg
    = TeamNameInputUpdated String
    | CreateTeam
    | EditPlayerName
    | SavePlayerName
    | PlayerNameInputUpdated String
    | JoinTeam String
    | DeleteTeam String
    | StartGame
    | LobbyStateUpdated Server.State.State


type PlayingMsg
    = Delta Float
    | ViewportChanged Browser.Dom.Viewport
    | MovementPadDown Vec2
    | MovementPadMoved Vec2
    | MovementPadUp
    | TargetPadDown Vec2
    | TargetPadMoved Vec2
    | TargetPadUp
    | BodiesUpdated Json.Encode.Value


type Msg
    = GameUpdated Json.Encode.Value
    | PreLobbyMsg PreLobbyMsg
    | LobbyMsg LobbyMsg
    | PlayingMsg PlayingMsg


fovAngle : Angle.Angle
fovAngle =
    Angle.degrees 30


initWorld : Physics.World.World Data
initWorld =
    Physics.World.empty
        |> Physics.World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ


isAlive : Physics.Body.Body Data -> Bool
isAlive body =
    let
        data =
            Physics.Body.data body
    in
    data.hp > 0


addBodies :
    List (Physics.Body.Body BodyData.Data)
    -> Physics.World.World BodyData.Data
    -> Physics.World.World BodyData.Data
addBodies bodies world =
    List.foldl Physics.World.add world bodies


init : Flags -> ( Model, Cmd Msg )
init { playerId } =
    ( ( playerId
      , PreLobby
            { gameCode = ""
            , topic = ""
            , playerId = playerId
            }
      )
    , Cmd.none
    )


initLobby : PlayerId -> Server.State.State -> ( Game, Cmd Msg )
initLobby playerId serverState =
    ( Lobby
        { playerId = playerId
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


initPlaying : PlayerId -> Server.State.State -> ( Game, Cmd Msg )
initPlaying playerId { bodies, lastUpdated, teams } =
    let
        arenaObstacles =
            List.filter (Physics.Body.data >> .class >> (==) Obstacle) bodies

        arena =
            addBodies arenaObstacles initWorld

        world =
            addBodies bodies initWorld

        initialCameraZRotationAngle =
            case getMe world of
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
    ( Playing
        { playerId = playerId
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
        }
    , Task.perform (PlayingMsg << ViewportChanged) Browser.Dom.getViewport
    )


initPostGame : PlayerId -> Server.State.State -> ( Game, Cmd Msg )
initPostGame playerId serverState =
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
    ( PostGame
        { playerId = playerId
        , teams = serverState.teams
        , players = playersWithStats
        , topic = serverState.topic
        }
    , Cmd.none
    )


jumpThresholdMs : Float
jumpThresholdMs =
    300.0


shootThresholdMs : Float
shootThresholdMs =
    300.0


updateMovement : Float -> Maybe (Physics.Body.Body BodyData.Data) -> PlayingState -> ( PlayingState, Cmd PlayingMsg )
updateMovement ms maybeMe gameState =
    case gameState.movementPadOffset of
        Just relativePos ->
            let
                direction =
                    Vec2.direction relativePos gameState.movementPadOrigin

                { x, y } =
                    Vec2.toRecord <| direction

                xMovement =
                    x / 10

                yMovement =
                    y / 10

                coordsAreInvalid =
                    isNaN x || isNaN y

                ( gameStateWithUpdatedWorld, cmds ) =
                    case ( maybeMe, coordsAreInvalid ) of
                        ( Just me, False ) ->
                            let
                                myFrame =
                                    Physics.Body.frame me

                                maybeNextZ =
                                    getMe gameState.world |> Maybe.map (Physics.Body.originPoint >> Point3d.zCoordinate >> Length.inMeters)

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
                                    updateMe (Physics.Body.moveTo nextOriginPoint) gameState.world
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


updateTarget : Float -> PlayingState -> ( PlayingState, Cmd PlayingMsg )
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


applyActions : Maybe (Physics.Body.Body BodyData.Data) -> PlayingState -> Float -> ( PlayingState, Cmd PlayingMsg )
applyActions previousMe gameState ms =
    let
        playerIsAlive =
            case getMe gameState.world of
                Nothing ->
                    False

                Just me ->
                    isAlive me

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


applyJump : PlayingState -> ( PlayingState, Cmd PlayingMsg )
applyJump gameState =
    ( gameState
    , requestJump gameState.playerId
    )


applyShoot : PlayingState -> ( PlayingState, Cmd PlayingMsg )
applyShoot gameState =
    let
        projectileOrientation =
            getLookAxis gameState.world gameState.cameraXRotationAngle gameState.cameraZRotationAngle

        projectileStartPoint =
            Point3d.along projectileOrientation (Length.centimeters 30)

        projectile =
            Physics.Body.particle { class = Bullet, mesh = WebGL.triangles [], hp = 0 }
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.grams 50))
                |> Physics.Body.moveTo projectileStartPoint
                |> Physics.Body.applyImpulse
                    (Force.newtons 200 |> Quantity.times (Duration.milliseconds 16.667))
                    (Axis3d.direction projectileOrientation)
                    projectileStartPoint
    in
    ( gameState
    , requestShoot
        { playerId = gameState.playerId
        , position = Point3d.toRecord Length.inMeters projectileStartPoint
        , linvel =
            projectile
                |> Physics.Body.velocity
                |> Vector3d.for (Duration.seconds 1)
                |> Vector3d.toRecord Length.inMeters
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( playerId, game ) =
    let
        ( nextGame, nextCmd ) =
            case ( msg, game ) of
                ( GameUpdated gameUpdateData, _ ) ->
                    case Json.Decode.decodeValue (Server.State.decoder playerId) gameUpdateData of
                        Err _ ->
                            ( game, Cmd.none )

                        Ok serverState ->
                            case ( game, serverState.status ) of
                                ( PreLobby _, _ ) ->
                                    initLobby playerId serverState

                                ( Lobby _, Server.State.Playing ) ->
                                    initPlaying playerId serverState

                                ( Lobby lobbyState, _ ) ->
                                    let
                                        ( nextLobbyState, cmds ) =
                                            updateLobby (LobbyStateUpdated serverState) lobbyState
                                    in
                                    ( Lobby nextLobbyState, Cmd.map LobbyMsg cmds )

                                ( Playing _, Server.State.Finished ) ->
                                    initPostGame playerId serverState

                                ( Playing playingState, _ ) ->
                                    ( Playing playingState, Cmd.none )

                                ( PostGame _, _ ) ->
                                    initPostGame playerId serverState

                ( PreLobbyMsg preLobbyMsg, PreLobby preLobbyState ) ->
                    let
                        ( nextPreLobbyState, cmds ) =
                            updatePreLobby preLobbyMsg preLobbyState
                    in
                    ( PreLobby nextPreLobbyState, Cmd.map PreLobbyMsg cmds )

                ( LobbyMsg lobbyMsg, Lobby lobbyState ) ->
                    let
                        ( nextLobbyState, cmds ) =
                            updateLobby lobbyMsg lobbyState
                    in
                    ( Lobby nextLobbyState, Cmd.map LobbyMsg cmds )

                ( PlayingMsg playingMsg, Playing playingState ) ->
                    let
                        ( nextPlayingState, cmds ) =
                            updatePlaying playingMsg playingState
                    in
                    ( Playing nextPlayingState, Cmd.map PlayingMsg cmds )

                ( _, PostGame postGameState ) ->
                    ( PostGame postGameState, Cmd.none )

                _ ->
                    ( game, Cmd.none )
    in
    ( ( playerId, nextGame ), nextCmd )


simulatePhysics : Float -> Physics.World.World Data -> Physics.World.World Data
simulatePhysics ms world =
    if useClientPhysics then
        Physics.World.simulate (Duration.milliseconds ms) world

    else
        world


getNextSimulatedPlayingState : Float -> PlayingState -> PlayingState
getNextSimulatedPlayingState ms gameState =
    { gameState
        | world = simulatePhysics ms gameState.world
        , newBodiesUpdate = Nothing
        , msSinceLastServerUpdateApplied = gameState.msSinceLastServerUpdateApplied + ms
    }


updatePreLobby : PreLobbyMsg -> PreLobbyState -> ( PreLobbyState, Cmd PreLobbyMsg )
updatePreLobby msg preLobbyState =
    case msg of
        GameCodeInputUpdated newGameCode ->
            ( { preLobbyState | gameCode = newGameCode }, Cmd.none )

        TopicUpdated newTopicName ->
            ( { preLobbyState | topic = newTopicName }, Cmd.none )

        JoinGame ->
            ( preLobbyState, joinGame { playerId = preLobbyState.playerId, gameId = Just preLobbyState.gameCode, topic = Nothing } )

        CreateGame ->
            ( preLobbyState, joinGame { playerId = preLobbyState.playerId, gameId = Nothing, topic = Just preLobbyState.topic } )


updateLobby : LobbyMsg -> LobbyState -> ( LobbyState, Cmd LobbyMsg )
updateLobby msg lobbyState =
    case msg of
        TeamNameInputUpdated newTeamName ->
            ( { lobbyState | teamNameInput = newTeamName }, Cmd.none )

        CreateTeam ->
            ( { lobbyState | teamNameInput = "" }, createTeam { playerId = lobbyState.playerId, name = lobbyState.teamNameInput } )

        LobbyStateUpdated nextGameData ->
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


updatePlaying : PlayingMsg -> PlayingState -> ( PlayingState, Cmd PlayingMsg )
updatePlaying msg gameState =
    case msg of
        Delta ms ->
            let
                nextModel =
                    case Maybe.map (Json.Decode.decodeValue (Server.State.bodiesStateDecoder gameState.playerId)) gameState.newBodiesUpdate of
                        Just (Ok { bodies, lastUpdated }) ->
                            let
                                isNewServerState =
                                    lastUpdated > gameState.lastUpdated

                                clientServerDiscrepancyMs =
                                    (toFloat gameState.lastUpdated + gameState.msSinceLastServerUpdateApplied) - toFloat lastUpdated
                            in
                            if isNewServerState then
                                let
                                    defaultNextWorld =
                                        gameState.arena
                                            |> addBodies bodies

                                    nextWorld =
                                        case ( getMe gameState.world, getMeInBodies bodies ) of
                                            ( Just oldMe, Just newMe ) ->
                                                let
                                                    nextMe =
                                                        Physics.Body.Extra.updateServerAuthoritativeData oldMe newMe
                                                in
                                                updateMe (always nextMe) defaultNextWorld

                                            _ ->
                                                defaultNextWorld
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
                    applyActions (getMe gameState.world) nextModel ms
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
            ( { gameState | viewSize = shortDimension - 20.0 }, Cmd.none )

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


viewHealthBar :
    Physics.Body.Body BodyData.Data
    -> Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    -> Scene3d.Entity Physics.Coordinates.WorldCoordinates
viewHealthBar body viewPointXDirection =
    let
        hp =
            (Physics.Body.data >> .hp) body

        hpBarLength =
            2.0

        healthBarCenter =
            Point3d.translateIn Direction3d.z (Length.meters 1) <| getEyePoint body

        healthBarOrigin =
            Point3d.translateIn (Direction3d.reverse viewPointXDirection) (Length.meters (hpBarLength / 2.0)) healthBarCenter

        healthBarAxis =
            Axis3d.through healthBarOrigin viewPointXDirection

        getPoint forHp =
            Point3d.along healthBarAxis (Length.meters (toFloat forHp * hpBarLength / 10.0))

        points remaining =
            case remaining of
                0 ->
                    []

                someHp ->
                    getPoint someHp :: points (remaining - 1)

        mesh =
            Scene3d.Mesh.points { radius = Pixels.float 2 } (points hp)
    in
    Scene3d.mesh (Scene3d.Material.color Color.lightGreen) mesh


viewPlaying :
    Physics.World.World BodyData.Data
    -> List Server.Team.Team
    -> Float
    -> Float
    -> Float
    -> Html PlayingMsg
viewPlaying world teams cameraXRotationAngle cameraZRotationAngle viewSize =
    let
        viewpoint =
            case getMe world of
                Just me ->
                    let
                        eyePoint =
                            getEyePoint me

                        cameraAxis =
                            getLookAxis world cameraXRotationAngle cameraZRotationAngle

                        focalPoint =
                            Point3d.along cameraAxis (Length.meters 1)
                    in
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint = focalPoint
                        , upDirection = Direction3d.positiveZ
                        }

                Nothing ->
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.origin
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }

        camera =
            Camera3d.perspective { viewpoint = viewpoint, verticalFieldOfView = fovAngle }

        entities =
            world
                |> Physics.World.bodies
                |> List.filterMap
                    (\body ->
                        let
                            bodyFrame =
                                Physics.Body.frame body

                            bodyData =
                                Physics.Body.data body
                        in
                        case bodyData.class of
                            Bullet ->
                                Just <|
                                    (Scene3d.sphere
                                        (Scene3d.Material.matte Color.blue)
                                        (Sphere3d.atOrigin (Length.centimeters 5))
                                        |> Scene3d.placeIn bodyFrame
                                    )

                            Obstacle ->
                                Just <|
                                    (Scene3d.blockWithShadow
                                        (Scene3d.Material.nonmetal
                                            { baseColor =
                                                case bodyData.id of
                                                    "floor" ->
                                                        Color.lightBrown

                                                    _ ->
                                                        Color.lightCharcoal
                                            , roughness = 0.5
                                            }
                                        )
                                        (Block3d.centeredOn Frame3d.atOrigin
                                            (case bodyData.dimensions of
                                                Block x y z ->
                                                    ( Length.meters x, Length.meters y, Length.meters z )

                                                None ->
                                                    ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                            )
                                        )
                                        |> Scene3d.placeIn bodyFrame
                                    )

                            NPC ->
                                Just <|
                                    if bodyData.hp == 0 then
                                        Scene3d.blockWithShadow
                                            (Scene3d.Material.matte Color.lightGray)
                                            (Block3d.centeredOn
                                                Frame3d.atOrigin
                                                ( Length.meters 1.25, Length.meters 0.5, Length.meters 2.0 )
                                            )
                                            |> Scene3d.placeIn bodyFrame

                                    else
                                        let
                                            color =
                                                case List.Extra.find (\team -> bodyData.teamId == Just team.id) teams of
                                                    Just team ->
                                                        team.color

                                                    Nothing ->
                                                        Color.black

                                            entity =
                                                case Mesh.Player.maybeMesh of
                                                    Just playerMesh ->
                                                        Scene3d.group
                                                            [ Scene3d.mesh
                                                                (Scene3d.Material.metal { baseColor = color, roughness = 0.1 })
                                                                playerMesh
                                                            , Scene3d.cylinderWithShadow
                                                                (Scene3d.Material.metal { baseColor = Color.rgba 0 0 0 0, roughness = 0.1 })
                                                                (Cylinder3d.centeredOn
                                                                    Point3d.origin
                                                                    Direction3d.z
                                                                    { radius = Length.meters 0.4
                                                                    , length = Length.meters 1.8
                                                                    }
                                                                )
                                                            ]

                                                    Nothing ->
                                                        Scene3d.cylinderWithShadow
                                                            (Scene3d.Material.metal { baseColor = color, roughness = 0.1 })
                                                            (Cylinder3d.centeredOn
                                                                Point3d.origin
                                                                Direction3d.z
                                                                { radius = Length.meters 0.525
                                                                , length = Length.meters 2.0
                                                                }
                                                            )
                                        in
                                        Scene3d.group
                                            [ Scene3d.placeIn bodyFrame entity
                                            , viewHealthBar body (Viewpoint3d.xDirection viewpoint)
                                            ]

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
                , dimensions = ( Pixels.int <| round viewSize, Pixels.int <| round viewSize )
                , camera = camera
                , clipDepth = Length.meters 0.5
                , background = Scene3d.transparentBackground
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


viewPreLobby : PreLobbyState -> Html PreLobbyMsg
viewPreLobby preLobbyState =
    Html.div
        [ Html.Attributes.style "flex" "1"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "space-around"
        , Html.Attributes.style "height" "75%"
        , Html.Attributes.style "display" "flex"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            ]
            [ Html.div [] [ Html.text "Join a debate topic" ]
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
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            ]
            [ Html.div [] [ Html.text "OR" ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            ]
            [ Html.div [] [ Html.text "Start a new debate topic (like \"Where should we eat tonight?\")" ]
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


viewLobby : LobbyState -> Html LobbyMsg
viewLobby lobbyState =
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


view : Model -> Html Msg
view ( _, game ) =
    case game of
        PreLobby preLobbyState ->
            Html.map PreLobbyMsg <| viewPreLobby preLobbyState

        Lobby lobbyState ->
            Html.map LobbyMsg <| viewLobby lobbyState

        Playing playingState ->
            Html.map PlayingMsg <|
                Html.Lazy.lazy5 viewPlaying
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ case model of
                        ( _, Playing _ ) ->
                            Browser.Events.onAnimationFrameDelta (PlayingMsg << Delta)

                        _ ->
                            Sub.none
                    , gameUpdated GameUpdated
                    , bodiesUpdated (PlayingMsg << BodiesUpdated)
                    ]
        , update = update
        }


port joinGame : { playerId : String, gameId : Maybe String, topic : Maybe String } -> Cmd msg


port startGame : () -> Cmd msg


port createTeam : { playerId : String, name : String } -> Cmd msg


port joinTeam : { playerId : String, teamId : String } -> Cmd msg


port deleteTeam : String -> Cmd msg


port updatePlayerName : { playerId : String, name : String } -> Cmd msg


port requestJump : String -> Cmd msg


port requestMove : { playerId : String, x : Float, y : Float } -> Cmd msg


port requestRotate : { playerId : String, angle : Float } -> Cmd msg


port requestShoot : { playerId : String, position : { x : Float, y : Float, z : Float }, linvel : { x : Float, y : Float, z : Float } } -> Cmd msg


port gameUpdated : (Json.Encode.Value -> msg) -> Sub msg


port bodiesUpdated : (Json.Encode.Value -> msg) -> Sub msg
