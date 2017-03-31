module Main exposing (..)

import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Color exposing (Color)
import Svg
import Svg.Attributes exposing (..)
import AnimationFrame
import Window
import String


-- main : Program Never


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            (\_ ->
                Sub.batch
                    [ Window.resizes ResizeWindow
                    , AnimationFrame.times Tick
                    ]
            )
        }


type alias Model =
    { bodies : List Body
    , money : Int
    , window : Window.Size
    }


initialModel : Model
initialModel =
    { bodies =
        [ Planet
            { name = "Sun"
            , position = Coords 0 0
            , mass = 1000
            , radius = 100
            , color = Color.yellow
            , velocity = Vector 0 0
            , acceleration = Vector 0 0
            }
        , Planet
            { name = "Planet Zebulon"
            , mass = 5
            , position = Coords 200 150
            , radius = 5
            , color = Color.red
            , velocity = Vector (0.25 * Basics.pi) 2
            , acceleration = Vector 0 50
            }
        ]
    , money = 1000
    , window = { width = 1200, height = 800 }
    }


type Body
    = Planet
        { name : String
        , mass : Float
        , position : Coords
        , radius : Float
        , color : Color
        , velocity : Vector
        , acceleration : Vector --net acceleration
        }


type alias Coords =
    { x : Float
    , y : Float
    }


type Vector
    = Vector Angle Speed


{-| Units are always radians
-}
type alias Angle =
    Float


type alias Speed =
    Float


type Msg
    = NoOp
    | Tick Time
    | ResizeWindow Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                newBodies =
                    List.map updatePosition model.bodies

                newModel =
                    { model | bodies = newBodies }
            in
                ( newModel, Cmd.none )

        ResizeWindow size ->
            ( { model | window = size }
            , Cmd.none
            )


updatePosition body =
    case body of
        Planet planet ->
            let
                newCoords =
                    updateCoords planet.velocity planet.position
            in
                Planet { planet | position = newCoords }


updateCoords (Vector angle speed) coords =
    let
        ( dx, dy ) =
            fromPolar ( speed, angle )
    in
        { x = coords.x + dx
        , y = coords.y + dy
        }


distance : Coords -> Coords -> Float
distance p0 p1 =
    sqrt <| (p1.x - p0.x) ^ 2 + (p1.y - p0.y) ^ 2


angle : Coords -> Coords -> Float
angle p0 p1 =
    let
        dx =
            p1.x - p0.x

        dy =
            p1.y - p0.y

        ( r, theta ) =
            toPolar ( dx, dy )
    in
        theta


g =
    6.673 * 10 ^ -11



-- N m2/kg2


force : Body -> Body -> Vector
force b0 b1 =
    let
        ( p0, m0 ) =
            case b0 of
                Planet planet ->
                    ( planet.position, planet.mass )

        ( p1, m1 ) =
            case b1 of
                Planet planet ->
                    ( planet.position, planet.mass )

        dist =
            distance p0 p1

        theta =
            angle p0 p1

        f_g =
            (g * m0 * m1) / dist ^ 2
    in
        Vector theta f_g


view : Model -> Html Msg
view model =
    let
        left =
            round (-1 * (toFloat model.window.width / 2.0))

        top =
            round (-1 * (toFloat model.window.height / 2.0))
    in
        div
            [ Html.Attributes.style
                [ ( "background-image", "url(\"assets/saturatedbackground.jpg\")" )
                , ( "background-size", "100% 100%" )
                ]
            ]
            [ Svg.svg
                [ Svg.Attributes.width "100%"
                , Svg.Attributes.height "100%"
                , Svg.Attributes.viewBox (String.join " " <| List.map toString [ left, top, model.window.width, model.window.height ])
                ]
                (List.map viewBody model.bodies)
            , span
                [ Html.Attributes.style
                    [ ( "position", "fixed" )
                    , ( "top", "20px" )
                    , ( "left", "20px" )
                    , ( "color", "white" )
                    ]
                ]
                [ text ("money: " ++ toString model.money) ]
            ]


viewBody : Body -> Html Msg
viewBody body =
    case body of
        Planet planet ->
            Svg.g
                []
                [ Svg.circle
                    [ r (toString planet.radius)
                    , cx (toString <| round planet.position.x)
                    , cy (toString <| round planet.position.y)
                    , Html.Attributes.style
                        [ ( "fill", renderColor planet.color )
                        ]
                    ]
                    []
                , viewVector planet.position planet.acceleration
                ]


viewVector : Coords -> Vector -> Html Msg
viewVector pos (Vector angle scalar) =
    let
        ( dx, dy ) =
            fromPolar ( scalar, angle )

        targetX =
            pos.x + dx

        targetY =
            pos.y + dy
    in
        Svg.line
            [ x1 (toString pos.x)
            , y1 (toString pos.y)
            , x2 (toString targetX)
            , y2 (toString targetY)
            , stroke "pink"
            ]
            []


renderColor : Color -> String
renderColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ")"
