module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = Clicked


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    button [ onClick Clicked ] [ text "Click me!" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



