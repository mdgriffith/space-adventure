module Main exposing (..)

import Task
import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Color exposing (Color)
import Svg
import Svg.Attributes exposing (..)
import Window
import String


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Task.perform (\_ -> NoOp) ResizeWindow Window.size )
        , update = update
        , view = view
        , subscriptions = (\_ -> Window.resizes ResizeWindow)
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
            }
        , Planet
            { name = "Planet Zebulon"
            , mass = 5
            , position = Coords 200 150
            , radius = 5
            , color = Color.red
            , velocity = Vector 0 0
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
            ( model, Cmd.none )

        ResizeWindow size ->
            ( { model | window = size }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        left =
            round
                (-1
                    * (toFloat model.window.width / 2.0)
                )

        top =
            round
                (-1
                    * (toFloat model.window.height / 2.0)
                )
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
            Svg.circle
                [ r (toString planet.radius)
                , cx (toString <| round planet.position.x)
                , cy (toString <| round planet.position.y)
                , Html.Attributes.style
                    [ ( "fill", renderColor planet.color )
                    ]
                ]
                []


renderColor : Color -> String
renderColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ")"
