module Flupsicom exposing (view)


{-- Qualities are specific to a medium.
    While Ambilang can imply sensibilities (openness for q),
    a quality is not bound to a range or rendition.

    In the current implementation, we want the medium
    to only tag with classes. Later, we may add hrefs. --}


import Html exposing (a, h1, h2, h3, p, article, aside, button, section, span, li, ol, ul, Html, text, div, h1, img)

type alias Name = String
type alias Element = Html msg

{-- View converts a Group as Html, mixing in lower-level Html.
    Relations map to controls whereas concepts map to labels. --}

viewLabel : String -> Html msg

view : Name -> List (Html msg) -> Html msg
view name children
  = case name of
    "CV" -> article [class name] children
    "work" -> if (children == Id a [class name, href "#"++id]
    "title" -> h2 [class name] children
