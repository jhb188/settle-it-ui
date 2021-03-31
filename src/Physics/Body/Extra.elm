module Physics.Body.Extra exposing (getEyePoint)

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
