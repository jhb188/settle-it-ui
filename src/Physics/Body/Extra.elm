module Physics.Body.Extra exposing (getEyePoint, updateServerAuthoritativeData)

import BodyData exposing (Data)
import Length
import Physics.Body
import Physics.Coordinates
import Point3d
import Vector3d


getEyePoint : Physics.Body.Body Data -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
getEyePoint me =
    Physics.Body.originPoint me
        |> Point3d.translateBy (Vector3d.meters 0 0 0.5)


updateServerAuthoritativeData :
    Physics.Body.Body BodyData.Data
    -> Physics.Body.Body BodyData.Data
    -> Physics.Body.Body BodyData.Data
updateServerAuthoritativeData oldBody newBody =
    let
        { x, y, z } =
            newBody |> Physics.Body.originPoint |> Point3d.toRecord Length.inMeters

        oldOrigin =
            oldBody |> Physics.Body.originPoint |> Point3d.toRecord Length.inMeters

        updatedOrigin =
            Point3d.fromRecord Length.meters { x = oldOrigin.x, y = oldOrigin.y, z = z }
    in
    newBody
        |> Physics.Body.moveTo updatedOrigin
