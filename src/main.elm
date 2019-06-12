module Main exposing (main)

import Browser exposing (element)
import Checks exposing (..)
import Html exposing (Html, button, div, h2, i, span, table, td, text, th, thead, tr)
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
    , checkx : Maybe Checks
    }


init : String -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { counter = 0, text = flags, page = NonePage, checkx = Nothing }
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
                    ( { model | checkx = Just m }, Cmd.none )

                CheckPage ->
                    ( model, Cmd.none )

                ErgebnisPage ->
                    ( model, Cmd.none )
    in
    ( v, k )


view : Model -> Html Msg
view model =
    let
        header =
            viewHeader model.page
    in
    div []
        [ header
        , div
            [ class "row mt-5" ]
            [ div [ class "col" ] [ parseChecks model.checkx ]
            ]
        ]


viewHeader : Page -> Html Msg
viewHeader page =
    case page of
        NonePage ->
            headerText "None"

        CheckPage ->
            headerText "Checks"

        ErgebnisPage ->
            headerText "Ergebnisse"


headerText : String -> Html Msg
headerText s =
    h2 [ class "text-center mt-5" ] [ text s ]


parseChecks : Maybe Checks -> Html Msg
parseChecks m =
    case m of
        Nothing ->
            text "nothing"

        Just c ->
            checkTable c.checkList


checkTable : List Check -> Html Msg
checkTable cr =
    table [ class "table" ] <|
        List.concat
            [ [ thead [ class "thead-dark" ]
                    [ th [] [ text "ID" ]
                    , th [] [ text "Bezeichnung" ]
                    ]
              ]
            , List.map
                (\c ->
                    tr []
                        [ td [] [ text c.id ]
                        , td [] [ text c.checkName ]
                        ]
                )
                cr
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { counter = model.counter + 1, text = "neuer wert", page = NonePage, checkx = Nothing }, Cmd.none )
