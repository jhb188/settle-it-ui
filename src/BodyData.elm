module BodyData exposing (Class(..), Data, Dimensions(..), Vertex)

import Math.Vector3 exposing (Vec3)
import WebGL


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type Dimensions
    = Block Float Float Float
    | None


type Class
    = Me
    | NPC
    | Wall
    | Test
    | Bullet
    | Obstacle


type alias Data =
    { mesh : WebGL.Mesh Vertex
    , class : Class
    , hp : Int
    , id : String
    , teamId : Maybe String
    , dimensions : Dimensions
    }
