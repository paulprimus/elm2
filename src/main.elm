module Main exposing (main)

import Browser exposing (element)
import Checks exposing (..)
import Html exposing (Html, button, div, i, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)



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
    Sub.none


type Msg
    = Increment


type Page
    = NonePage
    | CheckPage
    | ErgebnisPage


type alias Model =
    { counter : Int
    , text : String
    , page : Page
    , checks : Maybe Checks
    }


init : String -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { counter = 0, text = flags, page = NonePage, checks = Nothing }
    in
    ( model, Cmd.none ) |> loadCurrentPage


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( v, k ) =
            case model.page of
                NonePage ->
                    let
                        m =
                            Checks.init
                    in
                    ( { model | checks = Just m }, Cmd.none )

                CheckPage ->
                    ( model, Cmd.none )

                ErgebnisPage ->
                    ( model, Cmd.none )
    in
    ( v, k )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col" ] [ parseChecks model.checks ]
        ]


parseChecks : Maybe Checks -> Html Msg
parseChecks m =
    case m of
        Nothing ->
            text "nothing"

        Just c ->
            List.map checkRow c.checks |> List.


checkRow : Check -> Html Msg
checkRow cr =
    div [ class "row" ]
        [ div [ class "col" ] [ text cr.id ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { counter = model.counter + 1, text = "neuer wert", page = NonePage, checks = Nothing }, Cmd.none )
