port module Main exposing (main)

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
subscriptions _ =
    selectedRoute Navkey


type Msg
    = Navkey String


type Page
    = NonePage
    | CheckPage
    | ErgebnisPage


port selectedRoute : (String -> msg) -> Sub msg


type alias Model =
    { page : Page
    , checkx : Maybe Checks
    }


init : String -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { page = NonePage, checkx = Nothing }
    in
    ( model, Cmd.none ) |> loadCurrentPage


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( v, k ) =
            case model.page of
                NonePage ->
                    ( model, Cmd.none )

                CheckPage ->
                    let
                        m =
                            Checks.init
                    in
                    ( { model | checkx = Just m }, Cmd.none )

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
            headerText "DQM Tool"

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


fromNavkey : String -> Page
fromNavkey navKey =
    case navKey of
        "checks" ->
            CheckPage

        "ergebnisse" ->
            NonePage

        _ ->
            NonePage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navkey key ->
            let
                page =
                    fromNavkey key
            in
            ( { page = page, checkx = Just { checkList = [ { id = "1", checkName = "asdfsdfa" }, { id = "2", checkName = "check 2" }, { id = "3", checkName = "check 3" } ] } }, Cmd.none )
