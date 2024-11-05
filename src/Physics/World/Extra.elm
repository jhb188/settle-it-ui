module Physics.World.Extra exposing (updateClass)

import BodyData exposing (Class(..), Data)
import Physics.Body
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
