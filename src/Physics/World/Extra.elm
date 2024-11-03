module Physics.World.Extra exposing (getLookAxis, getMe, getMeInBodies, updateMe)

import Angle
import Axis3d
import BodyData exposing (Class(..), Data)
import Frame3d
import Length
import List.Extra
import Physics.Body
import Physics.Body.Extra
import Physics.Coordinates
import Physics.World


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


getMeInBodies : List (Physics.Body.Body Data) -> Maybe (Physics.Body.Body Data)
getMeInBodies =
    List.Extra.find (hasClass Me)


getMe : Physics.World.World Data -> Maybe (Physics.Body.Body Data)
getMe =
    Physics.World.bodies >> getMeInBodies


getLookAxis : Physics.World.World BodyData.Data -> Float -> Float -> Axis3d.Axis3d Length.Meters Physics.Coordinates.WorldCoordinates
getLookAxis world xRotationDeg zRotationDeg =
    case getMe world of
        Just me ->
            Frame3d.atOrigin
                |> Frame3d.rotateAroundOwn
                    Frame3d.zAxis
                    (Angle.degrees zRotationDeg)
                |> Frame3d.rotateAroundOwn
                    Frame3d.xAxis
                    (Angle.degrees xRotationDeg)
                |> Frame3d.yAxis
                |> Axis3d.moveTo (Physics.Body.Extra.getEyePoint me)

        Nothing ->
            Frame3d.yAxis Frame3d.atOrigin
