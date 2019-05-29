module Checks exposing (Check, Checks, init, view)

import Html exposing (Html, div, text)


type alias Check =
    { id : String
    , checkName : String
    }


type alias Checks =
    { checks : List Check
    }


type Msg
    = Msg1


init : Checks
init =
    { checks = [ { id = "1", checkName = "asdfsdfa" } ] }


view : Checks -> Html Msg
view checks =
    div [] [ text "checks" ]
