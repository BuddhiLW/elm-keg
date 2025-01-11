module View exposing (View, map)

import Element exposing (Element)


type alias View msg =
    { title : String
    , attributes : List (Element.Attribute msg)
    , body : List (Element msg)
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn view =
    { title = view.title
    , attributes = List.map (Element.mapAttribute fn) view.attributes
    , body = List.map (Element.map fn) view.body
    }
