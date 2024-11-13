module Texture exposing (..)

import Physics.Coordinates
import Scene3d.Material
import Task
import WebGL.Texture


type alias TextureSource =
    { baseColorMap : String
    , roughnessMap : String
    , metallicMap : String
    , ambientOcclusionMap : String
    , normalMap : String
    }


type alias Material =
    Scene3d.Material.Textured Physics.Coordinates.BodyCoordinates


textureSourceToTexture :
    TextureSource
    -> Task.Task WebGL.Texture.Error Material
textureSourceToTexture { baseColorMap, roughnessMap, metallicMap } =
    Task.map3
        (\baseColor roughness metallic ->
            Scene3d.Material.texturedPbr
                { baseColor = baseColor
                , roughness = roughness
                , metallic = metallic
                }
        )
        (Scene3d.Material.loadWith Scene3d.Material.trilinearFiltering baseColorMap)
        (Scene3d.Material.loadWith Scene3d.Material.trilinearFiltering roughnessMap)
        (Scene3d.Material.loadWith Scene3d.Material.trilinearFiltering metallicMap)


getTextureSourceForId : String -> TextureSource
getTextureSourceForId id =
    let
        path =
            "%PUBLIC_URL%/textures/" ++ id ++ "/"
    in
    { baseColorMap = path ++ "basecolor.png"
    , roughnessMap = path ++ "roughness.png"
    , metallicMap = path ++ "metallic.png"
    , ambientOcclusionMap = path ++ "ambientocclusion.png"
    , normalMap = path ++ "normal.png"
    }


loadTexture : String -> Task.Task WebGL.Texture.Error Material
loadTexture =
    getTextureSourceForId >> textureSourceToTexture


type alias Textures =
    { ground : Material
    }


init : Task.Task WebGL.Texture.Error Textures
init =
    Task.map
        (\ground ->
            { ground = ground
            }
        )
        (loadTexture "ground")
