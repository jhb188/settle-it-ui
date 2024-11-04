module Server.Body exposing (Body, physicsBodiesDecoder)

import Angle
import Axis3d
import Block3d
import BodyData exposing (Class(..), Dimensions(..))
import Cylinder3d
import Direction3d
import Duration
import Force
import Frame3d
import Json.Decode
import Json.Decode.Pipeline
import Length
import Mass
import Physics.Body
import Point3d
import Quantity
import Sphere3d
import Vector3d
import WebGL


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Body =
    { id : String
    , teamId : Maybe String
    , translation : Vector
    , rotation : Vector
    , linearVelocity : Vector
    , angularVelocity : Vector
    , dimensions : Vector
    , mass : Float
    , class : Class
    , hp : Int
    }


vectorDecoder : Json.Decode.Decoder Vector
vectorDecoder =
    Json.Decode.map3 Vector
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
        (Json.Decode.field "z" Json.Decode.float)


classDecoder : Json.Decode.Decoder Class
classDecoder =
    Json.Decode.string
        |> Json.Decode.map
            (\class ->
                case class of
                    "player" ->
                        NPC

                    "bullet" ->
                        Bullet

                    "obstacle" ->
                        Obstacle

                    _ ->
                        Test
            )


decoder : Json.Decode.Decoder Body
decoder =
    Json.Decode.succeed Body
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "tid" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "tra" vectorDecoder
        |> Json.Decode.Pipeline.required "rot" vectorDecoder
        |> Json.Decode.Pipeline.required "lv" vectorDecoder
        |> Json.Decode.Pipeline.required "av" vectorDecoder
        |> Json.Decode.Pipeline.required "d" vectorDecoder
        |> Json.Decode.Pipeline.required "m" Json.Decode.float
        |> Json.Decode.Pipeline.required "cl" classDecoder
        |> Json.Decode.Pipeline.required "hp" Json.Decode.int


physicsBodiesDecoder : String -> Json.Decode.Decoder (List (Physics.Body.Body BodyData.Data))
physicsBodiesDecoder myId =
    Json.Decode.list (Json.Decode.map (toPhysicsBody myId) decoder)


toPhysicsBody : String -> Body -> Physics.Body.Body BodyData.Data
toPhysicsBody myId body =
    let
        isMe =
            body.id == myId

        unrotatedPhysicsBody =
            getDefaultBody isMe body
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.grams body.mass))
                |> Physics.Body.translateBy (Vector3d.meters body.translation.x body.translation.y body.translation.z)

        frame =
            Physics.Body.frame unrotatedPhysicsBody

        rotationAxis =
            Frame3d.zAxis frame |> Axis3d.placeIn Frame3d.atOrigin

        physicsBody =
            Physics.Body.rotateAround rotationAxis (Angle.degrees body.rotation.z) unrotatedPhysicsBody

        linearVelocityCoords =
            body.linearVelocity

        linearVelocityVector =
            Vector3d.meters linearVelocityCoords.x linearVelocityCoords.y linearVelocityCoords.z
    in
    case Vector3d.direction linearVelocityVector of
        Nothing ->
            physicsBody

        Just linearVelocityDirection ->
            let
                impulseDurationSeconds =
                    0.1

                velocity =
                    Vector3d.length linearVelocityVector |> Length.inMeters

                impulse =
                    Force.newtons (body.mass * velocity / impulseDurationSeconds)
                        -- hacky normalize for now because some linear velocity unit is in disagreement between
                        -- client and server
                        |> Quantity.divideBy 1000
                        |> Quantity.times (Duration.seconds impulseDurationSeconds)

                nextPhysicsBody =
                    physicsBody |> Physics.Body.applyImpulse impulse linearVelocityDirection Point3d.origin
            in
            nextPhysicsBody


getDefaultBody : Bool -> Body -> Physics.Body.Body BodyData.Data
getDefaultBody isMe body =
    case body.class of
        NPC ->
            Physics.Body.cylinder
                (Cylinder3d.centeredOn
                    Point3d.origin
                    Direction3d.z
                    { radius = Length.meters 0.525, length = Length.meter }
                )
                { mesh = WebGL.triangles []
                , class =
                    if isMe then
                        Me

                    else
                        NPC
                , hp = body.hp
                , id = body.id
                , teamId = body.teamId
                , dimensions = None
                }
                |> Physics.Body.withDamping { linear = 0.0, angular = 1.0 }

        Bullet ->
            Physics.Body.sphere
                (Sphere3d.atOrigin (Length.centimeters 5))
                { mesh = WebGL.triangles [], class = BodyData.Bullet, hp = 0, id = body.id, teamId = Nothing, dimensions = None }
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.grams 50))

        Obstacle ->
            let
                { x, y, z } =
                    body.dimensions
            in
            Physics.Body.block
                (Block3d.centeredOn
                    Frame3d.atOrigin
                    ( Length.meters x, Length.meters y, Length.meters z )
                )
                { mesh = WebGL.triangles [], class = BodyData.Obstacle, hp = 0, id = body.id, teamId = Nothing, dimensions = Block x y z }
                |> Physics.Body.withBehavior Physics.Body.static

        _ ->
            Physics.Body.sphere
                (Sphere3d.atOrigin (Length.feet 1))
                { mesh = WebGL.triangles [], class = BodyData.Test, hp = 0, id = "origin", teamId = Nothing, dimensions = None }
