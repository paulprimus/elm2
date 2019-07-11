port module Main exposing (main)

import Browser exposing (element)
import Checks
import Html exposing (Html, button, div, h2, i, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder, list, map2)
import Task



---- Program ----


main : Program String Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        MainPage ->
            selectedRoute Navkey

        CheckPage checklist ->
            selectedRoute Navkey


type Msg
    = Navkey String
    | ChecksMsg Checks.Msg


type Model
    = MainPage
    | CheckPage Checks.Model


port selectedRoute : (String -> msg) -> Sub msg


init : String -> ( Model, Cmd Msg )
init _ =
    let
        model =
            MainPage
    in
    ( model, Cmd.none )



-- loadPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
-- loadPage ( model, cmd ) =
--     ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        MainPage ->
            h2 [ class "text-center mt-5" ] [ text "DQM-Tool" ]

        CheckPage page ->
            Html.map ChecksMsg (Checks.view page)


fromNavkey : String -> Model -> ( Model, Cmd Msg )
fromNavkey navKey model =
    case navKey of
        "checks" ->
            Checks.init |> updateWith CheckPage ChecksMsg model

        "ergebnisse" ->
            ( MainPage, Cmd.none )

        "home" ->
            ( MainPage, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Navkey key, _ ) ->
            let
                _ =
                    Debug.log " update Navkey" key
            in
            fromNavkey key model

        --   Checks.init |> updateWith CheckPage ChecksMsg model
        ( ChecksMsg subMsg, CheckPage page ) ->
            let
                _ =
                    Debug.log " update ChecksMsg" subMsg
            in
            Checks.update subMsg page |> updateWith CheckPage ChecksMsg model

        ( _, _ ) ->
            ( model, Cmd.none )



--( MainPage, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
-- updateWith toModel toMsg model ( subModel, subCmd ) =
--     ( toModel subModel
--     , Cmd.map toMsg subCmd
--     )
