module Scene3d.Entity.Extra exposing (..)

import Angle
import Axis3d
import Block3d
import BodyData exposing (Dimensions(..))
import Color
import Common exposing (StandardUnits)
import Cylinder3d
import Direction3d
import Frame3d
import Length
import List.Extra
import Me exposing (getEyePoint)
import Mesh.Player
import Physics.Body
import Physics.Coordinates
import Pixels
import Point3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Server.Team
import Sphere3d
import Texture
import Viewpoint3d


type alias Body =
    Physics.Body.Body BodyData.Data


type alias Entity =
    Scene3d.Entity Physics.Coordinates.WorldCoordinates


bullet : Body -> Entity
bullet body =
    Scene3d.placeIn (Physics.Body.frame body) <|
        Scene3d.sphere
            (Scene3d.Material.matte Color.blue)
            (Sphere3d.atOrigin (Length.centimeters 5))


arenaWidth : Float
arenaWidth =
    200


getTiledFloor texture =
    let
        tileSize =
            10

        numTiles =
            round arenaWidth // tileSize

        entities =
            List.concatMap (\x -> List.map (getFloorTile texture tileSize x) (List.range 0 (numTiles - 1))) (List.range 0 (numTiles - 1))
    in
    Scene3d.group
        entities


floorHeight : Float
floorHeight =
    1.0


getFloorTile :
    Scene3d.Material.Textured Physics.Coordinates.BodyCoordinates
    -> Int
    -> Int
    -> Int
    -> Scene3d.Entity Physics.Coordinates.WorldCoordinates
getFloorTile texture intSize x y =
    let
        zPos =
            floorHeight / 2

        size =
            toFloat intSize

        offset =
            size / 2

        frontLeft =
            Point3d.meters 0 0 zPos

        frontRight =
            Point3d.meters size 0 zPos

        backLeft =
            Point3d.meters 0 size zPos

        backRight =
            Point3d.meters size size zPos

        baseline =
            -arenaWidth / 2

        position =
            Point3d.origin
                |> Point3d.translateIn Direction3d.x (Length.meters (toFloat x * size + baseline + offset))
                |> Point3d.translateIn Direction3d.y (Length.meters (toFloat y * size + baseline + offset))

        tileFrame =
            Frame3d.atPoint position
    in
    Scene3d.quad
        texture
        frontRight
        frontLeft
        backLeft
        backRight
        |> Scene3d.placeIn tileFrame


floor : Maybe Texture.Textures -> Body -> Entity
floor textures body =
    case textures of
        Just { ground } ->
            getTiledFloor ground

        Nothing ->
            obstacle body


obstacle : Body -> Entity
obstacle body =
    let
        bodyFrame =
            Physics.Body.frame body

        bodyData =
            Physics.Body.data body
    in
    Scene3d.placeIn bodyFrame <|
        Scene3d.blockWithShadow
            (Scene3d.Material.nonmetal
                { baseColor = Color.grey
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


npc :
    List Server.Team.Team
    -> Viewpoint3d.Viewpoint3d StandardUnits Physics.Coordinates.WorldCoordinates
    -> Body
    -> Entity
npc teams viewpoint body =
    let
        bodyFrame =
            Physics.Body.frame body

        bodyData =
            Physics.Body.data body
    in
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
            , healthBar body (Viewpoint3d.xDirection viewpoint)
            ]


healthBar :
    Body
    -> Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    -> Entity
healthBar body viewPointXDirection =
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

        points =
            List.range 1 hp |> List.map getPoint

        mesh =
            Scene3d.Mesh.points { radius = Pixels.float 2 } points
    in
    Scene3d.mesh (Scene3d.Material.color Color.lightGreen) mesh
