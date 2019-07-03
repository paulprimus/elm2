port module Main exposing (main)

import Browser exposing (element)
import Checks exposing (..)
import Html exposing (Html, button, div, h2, i, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder, list, map2)



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
    | ChecksReceived (Result Http.Error (List Check))


type Page
    = NonePage
    | CheckPage
    | ErgebnisPage
    | ErrorPage


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
    ( model, Cmd.none ) |> loadPage


loadPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadPage ( model, cmd ) =
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

                ErrorPage ->
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

        ErrorPage ->
            headerText "Fehler"


headerText : String -> Html Msg
headerText s =
    h2 [ class "text-center mt-5" ] [ text s ]


parseChecks : Maybe Checks -> Html Msg
parseChecks m =
    case m of
        Nothing ->
            text "Keine Checks gefunden!"

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
                        [ td [] [ text (String.fromInt c.id) ]
                        , td [] [ text c.checkName ]
                        ]
                )
                cr
            ]


checkDecoder : Decoder Check
checkDecoder =
    map2 Check
        (D.field "id" D.int)
        (D.field "checkName" D.string)


checkListDecoder : Decoder (List Check)
checkListDecoder =
    D.list checkDecoder


httpGetChecksCommand : Cmd Msg
httpGetChecksCommand =
    Http.get
        { url = "http://localhost:3000/checks"
        , expect = Http.expectJson ChecksReceived checkListDecoder
        }



-- fromNavkey : String -> Page
-- fromNavkey navKey =
--     case navKey of
--         "checks" ->
--             CheckPage
--         "ergebnisse" ->
--             NonePage
--         _ ->
--             NonePage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navkey key ->
            -- ( { page = CheckPage, checkx = Just { checkList = [ { id = "1", checkName = "asdfsdfa" } ] } }, Cmd.none )
            ( model, httpGetChecksCommand )

        ChecksReceived response ->
            case response of
                Ok checklist ->
                    ( { page = CheckPage, checkx = Just { checkList = checklist } }, Cmd.none )

                Err error ->
                    ( { model | page = ErrorPage }, Cmd.none )
