module Server.Body exposing (..)

import Angle
import Axis3d
import BodyData
import Cylinder3d
import Direction3d
import Duration
import Force
import Json.Decode
import Length
import Mass
import Physics.Body
import Point3d
import Quantity
import Sphere3d
import Vector3d
import WebGL


type alias Vector =
    { x : Float, y : Float, z : Float }


type alias Body =
    { id : Maybe String
    , translation : Vector
    , rotation : Vector
    , linearVelocity : Vector
    , angularVelocity : Vector
    , mass : Float
    }


vectorDecoder : Json.Decode.Decoder Vector
vectorDecoder =
    Json.Decode.map3 Vector
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
        (Json.Decode.field "z" Json.Decode.float)


decoder : Json.Decode.Decoder Body
decoder =
    Json.Decode.map6 Body
        (Json.Decode.field "id" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "translation" vectorDecoder)
        (Json.Decode.field "rotation" vectorDecoder)
        (Json.Decode.field "linvel" vectorDecoder)
        (Json.Decode.field "angvel" vectorDecoder)
        (Json.Decode.field "mass" Json.Decode.float)


bodiesDecoder : Json.Decode.Decoder (List Body)
bodiesDecoder =
    Json.Decode.list decoder


physicsBodiesDecoder : String -> Json.Decode.Decoder (List (Physics.Body.Body BodyData.Data))
physicsBodiesDecoder myId =
    Json.Decode.list (Json.Decode.map (toPhysicsBody myId) decoder)



-- F = ma
-- v = at
-- a = v / t
-- F = (mv / t)


toPhysicsBody : String -> Body -> Physics.Body.Body BodyData.Data
toPhysicsBody myId body =
    let
        physicsBody =
            getDefaultBody myId body
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.grams body.mass))
                |> Physics.Body.translateBy (Vector3d.meters body.translation.x body.translation.y body.translation.z)
                |> Physics.Body.rotateAround Axis3d.z (Angle.degrees body.rotation.z)

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


getDefaultBody : String -> Body -> Physics.Body.Body BodyData.Data
getDefaultBody myId body =
    case body.id of
        Just id ->
            Physics.Body.cylinder
                (Cylinder3d.centeredOn
                    Point3d.origin
                    Direction3d.z
                    { radius = Length.centimeters 20, length = Length.meter }
                )
                { mesh = WebGL.triangles []
                , class =
                    if id == myId then
                        BodyData.Me

                    else
                        BodyData.NPC
                }

        Nothing ->
            Physics.Body.sphere
                (Sphere3d.atOrigin (Length.feet 1))
                { mesh = WebGL.triangles [], class = BodyData.Test }