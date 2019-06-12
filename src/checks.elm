module Checks exposing (Check, Checks, init, view)

import Html exposing (Html, div, text)


type alias Check =
    { id : String
    , checkName : String
    }


type alias Checks =
    { checkList : List Check
    }


type Msg
    = Msg1


init : Checks
init =
    { checkList = [ { id = "1", checkName = "asdfsdfa" }, { id = "2", checkName = "check 2" }, { id = "3", checkName = "check 3" } ] }


view : Checks -> Html Msg
view checks =
    div [] [ text "checks" ]
