module Physics.Body.Extra exposing (toSceneEntity)

import BodyData exposing (Class(..), Data, Dimensions(..))
import Common exposing (StandardUnits)
import Physics.Body
import Physics.Coordinates
import Scene3d
import Scene3d.Entity.Extra
import Server.Team
import Texture
import Viewpoint3d


toSceneEntity :
    Maybe Texture.Textures
    -> List Server.Team.Team
    -> Viewpoint3d.Viewpoint3d StandardUnits Physics.Coordinates.WorldCoordinates
    -> Physics.Body.Body Data
    -> Maybe (Scene3d.Entity Physics.Coordinates.WorldCoordinates)
toSceneEntity textures teams viewpoint body =
    let
        bodyData =
            Physics.Body.data body
    in
    case bodyData.class of
        Bullet ->
            Just <| Scene3d.Entity.Extra.bullet body

        Obstacle ->
            let
                entity =
                    case bodyData.id of
                        "floor" ->
                            Scene3d.Entity.Extra.floor textures body

                        _ ->
                            Scene3d.Entity.Extra.obstacle body
            in
            Just entity

        NPC ->
            Just <| Scene3d.Entity.Extra.npc teams viewpoint body

        _ ->
            Nothing
