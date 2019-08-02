module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (class, href, id, type_)
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadChecks


type alias Check =
    { id : Int
    , checkName : String
    , beschreibung : String
    , sqlQuery : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , checklist : List Check
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        LoadChecks ->
            ( model, Cmd.none )


viewNavibar : Model -> Html Msg
viewNavibar model =
    div [ class "navbar navbar-expand-md navbar-dark bg-dark" ]
        [ a [ class "navbar-brand" ] [ text "DQM" ]

        --   , button [ class "navbar-toggler", type_ "button" ] [ text "button" ]
        , div [ class "collapse navbar-collapse", id "navbarSupportedContent" ]
            [ ul [ class "navbar-nav mr-auto" ]
                [ viewLink "/home"
                , viewLink "/checks"
                , viewLink "/ergebnisse"
                ]
            ]
        ]


viewLink : String -> Html msg
viewLink path =
    li [ class "nav-link" ] [ a [ href path ] [ text path ] ]


viewContent model =
    div [ class "text-center" ] [ text "DQM Home" ]



--  <nav class="navbar navbar-expand-md navbar-dark bg-dark">
-- 	<a class="navbar-brand" href="#" id="nav-home">DQM</a>
-- 	<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent"
-- 	 aria-expanded="false" aria-label="Toggle navigation">
-- 		<span class="navbar-toggler-icon"></span>
-- 	</button>
-- 	<div class="collapse navbar-collapse" id="navbarSupportedContent">
-- 		<ul class="navbar-nav mr-auto">
-- 			<li id="nav-item-checks" class="nav-item"><a class="nav-link" href="#">Checks</a></li>
-- 			<li id="nav-item-ergebnisse" class="nav-item"><a class="nav-link" href="#">Ergebnisse</a></li>
-- 		</ul>
-- 	</div>
-- </nav>
--


view : Model -> Browser.Document Msg
view model =
    { title = "DQM-Tool "
    , body = [ viewNavibar model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [], Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
