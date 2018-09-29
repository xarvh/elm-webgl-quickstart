module Scene exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Meshes
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



-- Shader records


type alias Uniforms =
    { entityToCamera : Mat4
    }


type alias Varyings =
    { vfog : Float
    , vnormal : Vec3
    }


type alias VertexAttributes =
    Meshes.PlainVertex


vertexShader : Shader Meshes.PlainVertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 normal;
        attribute vec3 position;

        uniform mat4 entityToCamera;

        varying float vfog;
        varying vec3 vnormal;

        void main () {
            gl_Position = entityToCamera * vec4(position, 1.0);
            vfog = length(gl_Position.xyz) / 10.0;
            vnormal = normalize((entityToCamera * vec4(normal, 1.0)).xyz);
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        varying float vfog;
        varying vec3 vnormal;

        vec4 heroColor = vec4(0.0, 0.0, 1.0, 1.0);
        vec3 lightDirection = vec3(1.0, -1.0, -1.0);

        vec4 white = vec4(1.0);
        vec4 black = vec4(0.0, 0.0, 0.0, 1.0);
        vec4 fogColor = white;

        void main() {
            float lightIntensity = 0.5 + 0.5 * dot(vnormal, -1.0 * lightDirection);

            vec4 colorWithLight = mix(black, heroColor, lightIntensity);

            gl_FragColor = mix(colorWithLight, fogColor, vfog);
        }
    |]



-- Entities


type alias EntitiesArgs =
    { worldToCamera : Mat4
    , mousePosition : Vec2
    , time : Float
    }


entities : EntitiesArgs -> List Entity
entities { worldToCamera, mousePosition, time } =
    let
        entityToCamera =
            worldToCamera
              |> Mat4.rotate 0.2 (vec3 -0.5 -0.5 0.5)

        uniforms =
            { entityToCamera = entityToCamera
            }
    in
    [ WebGL.entity
        vertexShader
        fragmentShader
        Meshes.cube
        uniforms
    ]
