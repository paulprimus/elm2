module Checks exposing (Check, Model, Msg, init, update, view)

import Html exposing (Html, button, div, h2, i, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_)
import Http
import Json.Decode as D exposing (Decoder, list, map2)


type alias Check =
    { id : Int
    , checkName : String
    }


type alias Model =
    { checkList : List Check
    }


type Msg
    = LoadChecks
    | ReceivedChecks (Result Http.Error (List Check))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { checkList = [] }, httpGetChecksCommand )


view : Model -> Html Msg
view checks =
    let
        header =
            h2 [ class "text-center mt-5" ] [ text "DQM-Tool" ]
    in
    div []
        [ header
        , div
            [ class "row mt-5" ]
            [ div [ class "col" ] [ parseChecks checks ]
            ]
        ]


parseChecks : Model -> Html Msg
parseChecks model =
    -- case m of
    -- Nothing ->
    --     text "Keine Checks gefunden!"
    -- Just c ->
    checkTable model.checkList


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadChecks ->
            ( model, httpGetChecksCommand )

        ReceivedChecks response ->
            case response of
                Ok checklist ->
                    ( { checkList = checklist }, Cmd.none )

                Err error ->
                    ( { checkList = [] }, Cmd.none )


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
        , expect = Http.expectJson ReceivedChecks checkListDecoder
        }
