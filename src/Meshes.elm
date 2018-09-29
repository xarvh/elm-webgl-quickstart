module Meshes exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


type alias PlainVertex =
    { normal : Vec3
    , position : Vec3
    }


cube : Mesh PlainVertex
cube =
    let
        size =
            1

        half =
            size / 2

        vertex x y z =
            vec3 x y z

        -- X
        left =
            -half

        right =
            half

        -- Y
        top =
            size

        bottom =
            0

        -- Z
        front =
            half

        back =
            -half

        -- vertexes
        ltf =
            vertex left top front

        ltb =
            vertex left top back

        lbf =
            vertex left bottom front

        lbb =
            vertex left bottom back

        rtf =
            vertex right top front

        rtb =
            vertex right top back

        rbf =
            vertex right bottom front

        rbb =
            vertex right bottom back

        addNormalToTriangle : Vec3 -> ( Vec3, Vec3, Vec3 ) -> ( PlainVertex, PlainVertex, PlainVertex )
        addNormalToTriangle n ( a, b, c ) =
            ( { normal = Vec3.normalize (n), position = a }
            , { normal = Vec3.normalize (n), position = b }
            , { normal = Vec3.normalize (n), position = c }
            )

        -- ( { normal = Vec3.normalize(a), position = a }
        -- , { normal = Vec3.normalize(b), position = b }
        -- , { normal = Vec3.normalize(c), position = c }
        -- )
    in
        WebGL.triangles
            -- front
            [ addNormalToTriangle (vec3 0 0 front) ( ltf, lbf, rtf )
            , addNormalToTriangle (vec3 0 0 front) ( rtf, rbf, lbf )

            -- back
            , addNormalToTriangle (vec3 0 0 back) ( ltb, lbb, rtb )
            , addNormalToTriangle (vec3 0 0 back) ( rtb, rbb, lbb )

            -- left
            , addNormalToTriangle (vec3 left 0 0) ( ltf, ltb, lbf )
            , addNormalToTriangle (vec3 left 0 0) ( lbf, lbb, ltb )

            -- right
            , addNormalToTriangle (vec3 right 0 0) ( rtf, rtb, rbf )
            , addNormalToTriangle (vec3 right 0 0) ( rbf, rbb, rtb )

            -- top
            , addNormalToTriangle (vec3 0 top 0) ( ltf, ltb, rtf )
            , addNormalToTriangle (vec3 0 top 0) ( ltb, rtb, rtf )

            -- bottom
            , addNormalToTriangle (vec3 0 bottom 0) ( lbf, lbb, rbf )
            , addNormalToTriangle (vec3 0 bottom 0) ( lbb, rbb, rbf )
            ]
