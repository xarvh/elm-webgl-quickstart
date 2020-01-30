module Scene exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Mesh, Shader)



-- Periodic functions


periodLinear : Float -> Float -> Float -> Float
periodLinear time phase period =
    let
        t =
            time + phase * period

        n =
            t / period |> floor |> toFloat
    in
    t / period - n


periodHarmonic : Float -> Float -> Float -> Float
periodHarmonic time phase period =
    2 * pi * periodLinear time phase period |> sin



-- Square mesh


mesh : WebGL.Mesh CheckboardAttributes
mesh =
    WebGL.triangleFan
        [ { x = -0.5
          , y = -0.5
          }
        , { x = 0.5
          , y = -0.5
          }
        , { x = 0.5
          , y = 0.5
          }
        , { x = -0.5
          , y = 0.5
          }
        ]



-- Checkboard


type alias CheckboardAttributes =
    { x : Float
    , y : Float
    }


type alias CheckboardUniforms =
    { entityToCamera : Mat4
    , squares : Vec2
    , black : Vec4
    , white : Vec4
    }


type alias CheckboardVaryings =
    { position : Vec4
    }


checkboardVertexShader : WebGL.Shader CheckboardAttributes CheckboardUniforms CheckboardVaryings
checkboardVertexShader =
    [glsl|
        precision mediump float;

        attribute float x;
        attribute float y;
        uniform mat4 entityToCamera;
        uniform vec4 black;
        uniform vec4 white;
        varying vec4 position;

        void main () {
            position = vec4(x, y, 0, 1);
            gl_Position = entityToCamera * position;
        }
    |]


checkboardFragmentShader : WebGL.Shader {} CheckboardUniforms CheckboardVaryings
checkboardFragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 entityToCamera;
        uniform vec2 squares;
        uniform vec4 black;
        uniform vec4 white;
        varying vec4 position;

        void main () {
          float x = floor(position.x * squares.x);
          float y = floor(position.y * squares.y);
          gl_FragColor = mod(x + y, 2.0) == 0.0 ? black : white;
        }
    |]



-- Entities


type alias EntitiesArgs =
    { worldToCamera : Mat4
    , mousePosition : Vec2
    , worldSize : { width : Float, height : Float }
    , time : Float
    }


entities : EntitiesArgs -> List Entity
entities { worldToCamera, mousePosition, time, worldSize } =
    let
        black =
            0.7

        squaresPerWordUnit =
            10

        checkUniforms =
            { entityToCamera = Mat4.scale3 worldSize.width worldSize.height 1.0 worldToCamera
            , squares = vec2 (worldSize.width * squaresPerWordUnit) (worldSize.height * squaresPerWordUnit)
            , black = vec4 black black black 1
            , white = vec4 1 1 1 1
            }
    in
    [ WebGL.entity
        checkboardVertexShader
        checkboardFragmentShader
        mesh
        checkUniforms
    ]
