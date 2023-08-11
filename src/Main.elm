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


updateSnake : Snake -> Direction -> Snake
updateSnake snake direction =
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
                    case direction of
                        Up ->
                            List.append (head :: tail) [ { x = tailPixel.x, y = tailPixel.y + 1 } ]

                        Down ->
                            List.append (head :: tail) [ { x = tailPixel.x, y = tailPixel.y - 1 } ]

                        Right ->
                            List.append (head :: tail) [ { x = tailPixel.x + 1, y = tailPixel.y } ]

                        Left ->
                            List.append (head :: tail) [ { x = tailPixel.x - 1, y = tailPixel.y + 1 } ]


update : Msg -> Model -> Model
update msg model =
    { direction = msg, snake = updateSnake model.snake msg }



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
