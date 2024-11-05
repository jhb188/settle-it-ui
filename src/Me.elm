module Me exposing (..)

-- import Physics.Body.Extra

import Angle
import Axis3d
import BodyData exposing (Class(..), Data)
import Common exposing (StandardUnits)
import Direction3d
import Frame3d
import Length
import List.Extra
import Physics.Body
import Physics.Coordinates
import Physics.World
import Physics.World.Extra exposing (updateClass)
import Point3d
import Vector3d
import Viewpoint3d


getEyePoint :
    Physics.Body.Body Data
    -> Point3d.Point3d StandardUnits Physics.Coordinates.WorldCoordinates
getEyePoint me =
    Physics.Body.originPoint me
        |> Point3d.translateBy (Vector3d.meters 0 0 0.5)


updateInWorld : (Physics.Body.Body Data -> Physics.Body.Body Data) -> Physics.World.World Data -> Physics.World.World Data
updateInWorld =
    updateClass Me


getInBodies : List (Physics.Body.Body Data) -> Maybe (Physics.Body.Body Data)
getInBodies =
    List.Extra.find (Physics.Body.data >> .class >> (==) Me)


getInWorld : Physics.World.World Data -> Maybe (Physics.Body.Body Data)
getInWorld =
    Physics.World.bodies >> getInBodies


getLookAxis : Physics.World.World BodyData.Data -> Float -> Float -> Axis3d.Axis3d Length.Meters Physics.Coordinates.WorldCoordinates
getLookAxis world xRotationDeg zRotationDeg =
    case getInWorld world of
        Just me ->
            Frame3d.atOrigin
                |> Frame3d.rotateAroundOwn
                    Frame3d.zAxis
                    (Angle.degrees zRotationDeg)
                |> Frame3d.rotateAroundOwn
                    Frame3d.xAxis
                    (Angle.degrees xRotationDeg)
                |> Frame3d.yAxis
                |> Axis3d.moveTo (getEyePoint me)

        Nothing ->
            Frame3d.yAxis Frame3d.atOrigin


getViewpoint :
    Physics.World.World Data
    -> Float
    -> Float
    -> Viewpoint3d.Viewpoint3d StandardUnits Physics.Coordinates.WorldCoordinates
getViewpoint world cameraXRotationAngle cameraZRotationAngle =
    case getInWorld world of
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
