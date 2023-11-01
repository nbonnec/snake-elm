module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Attr
import Time



-- MAIN


main =
    Browser.element
        { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type Direction
    = Up
    | Down
    | Right
    | Left


type alias Pixel =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Pixel


maxLength =
    20


nextPixel : Direction -> Pixel -> Pixel
nextPixel direction pixel =
    case direction of
        Up ->
            { x = pixel.x, y = min (pixel.y + 1) maxLength }

        Down ->
            { x = pixel.x, y = max (pixel.y - 1) 0 }

        Right ->
            { x = min (pixel.x + 1) maxLength, y = pixel.y }

        Left ->
            { x = max (pixel.x - 1) 0, y = pixel.y }


type alias Model =
    { snake : Snake
    , direction : Direction
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { snake =
            [ { x = 5, y = 5 }
            , { x = 6, y = 6 }
            , { x = 7, y = 5 }
            ]
      , direction = Right
      }
    , Cmd.none
    )



-- STYLE


background : Html msg
background =
    Svg.svg
        [ Attr.viewBox "0 0 400 400"
        , Attr.width "400"
        , Attr.height "400"
        ]
        [ Svg.rect
            [ Attr.x "0"
            , Attr.y "0"
            , Attr.width "400"
            , Attr.height "400"
            , Attr.fill "green"
            , Attr.stroke "black"
            , Attr.strokeWidth "2"
            ]
            []
        ]



-- UPDATE


type Msg
    = Key Direction
    | Apple
    | Tick Time.Posix


addOnePixel : Snake -> Direction -> Snake
addOnePixel snake direction =
    let
        tailRecord =
            List.head (List.reverse snake)
    in
    case tailRecord of
        Nothing ->
            []

        Just tailPixel ->
            List.append snake [ nextPixel direction tailPixel ]


updateSnake : Model -> Snake
updateSnake model =
    case model.snake of
        [] ->
            []

        _ :: tail ->
            addOnePixel tail model.direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key newDirection ->
            case ( newDirection, model.direction ) of
                ( Up, Down ) ->
                    ( model, Cmd.none )

                ( Down, Up ) ->
                    ( model, Cmd.none )

                ( Left, Right ) ->
                    ( model, Cmd.none )

                ( Right, Left ) ->
                    ( model, Cmd.none )

                ( _, _ ) ->
                    ( { model | direction = newDirection }
                    , Cmd.none
                    )

        Apple ->
            ( { model | snake = addOnePixel model.snake model.direction }, Cmd.none )

        Tick _ ->
            ( { model | snake = updateSnake model }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ background
        , div [] [ text (List.foldr (\x a -> "{ " ++ String.fromInt x.x ++ ", " ++ String.fromInt x.y ++ "}, " ++ a) "" model.snake) ]
        , button [ onClick (Key Up) ] [ text "⬆️" ]
        , div [] []
        , button [ onClick (Key Left) ] [ text "⬅️" ]
        , button [ onClick (Key Down) ] [ text "⬇️" ]
        , button [ onClick (Key Right) ] [ text "➡️" ]
        , button [ onClick Apple ] [ text "🍎️" ]
        ]
