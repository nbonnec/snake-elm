module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time



-- MAIN


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



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


nextPixel : Direction -> Pixel -> Pixel
nextPixel direction pixel =
    case direction of
        Up ->
            { x = pixel.x, y = pixel.y + 1 }

        Down ->
            { x = pixel.x, y = pixel.y - 1 }

        Right ->
            { x = pixel.x + 1, y = pixel.y }

        Left ->
            { x = pixel.x - 1, y = pixel.y }


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



-- UPDATE


type Msg
    = Key Direction
    | Tick Time.Posix


updateSnake : Model -> Snake
updateSnake model =
    case model.snake of
        [] ->
            []

        head :: tail ->
            let
                tailRecord =
                    List.head (List.reverse tail)
            in
            case tailRecord of
                Nothing ->
                    []

                Just tailPixel ->
                    List.append (head :: tail) [ nextPixel model.direction tailPixel ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key direction ->
            ( { model | direction = direction }
            , Cmd.none
            )

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
        [ div [] [ text (List.foldr (\x a -> "{ " ++ String.fromInt x.x ++ ", " ++ String.fromInt x.y ++ "}, " ++ a) "" model.snake) ]
        , button [ onClick (Key Up) ] [ text "⬆️" ]
        , div [] []
        , button [ onClick (Key Left) ] [ text "⬅️" ]
        , button [ onClick (Key Down) ] [ text "⬇️" ]
        , button [ onClick (Key Right) ] [ text "➡️" ]
        ]
