module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



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


duplicatePixel : Direction -> Pixel -> Pixel
duplicatePixel direction pixel =
    case direction of
        Up ->
            { x = pixel.x, y = pixel.y + 1 }

        Down ->
            { x = pixel.x, y = pixel.y + 1 }

        Right ->
            { x = pixel.x, y = pixel.y + 1 }

        Left ->
            { x = pixel.x, y = pixel.y + 1 }


type alias Model =
    { snake : Snake
    , direction : Direction
    }


init : Model
init =
    { snake =
        [ { x = 5, y = 5 }
        , { x = 6, y = 6 }
        , { x = 7, y = 5 }
        ]
    , direction = Right
    }



-- UPDATE


type alias Msg =
    Direction


updateSnake : Direction -> Snake -> Snake
updateSnake direction snake =
    case snake of
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
                    List.append (head :: tail) [ duplicatePixel direction tailPixel ]


update msg model =
    { model | direction = msg, snake = updateSnake msg model.snake }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (List.foldr (\x a -> "{ " ++ String.fromInt x.x ++ ", " ++ String.fromInt x.y ++ "}, " ++ a) "" model.snake) ]
        , button [ onClick Up ] [ text "⬆️" ]
        , div [] []
        , button [ onClick Left ] [ text "⬅️" ]
        , button [ onClick Down ] [ text "⬇️" ]
        , button [ onClick Right ] [ text "➡️" ]
        ]
