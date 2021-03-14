module BodyData exposing (Class(..), Data, Vertex)

import Math.Vector3 exposing (Vec3)
import WebGL


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type Class
    = Me
    | NPC
    | Wall
    | Floor
    | Test
    | Bullet


type alias Data =
    { mesh : WebGL.Mesh Vertex
    , class : Class
    }
