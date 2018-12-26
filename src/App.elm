module App exposing
  (App, initial, Symbol(..), G(..), view, viewSymbol, viewDescription)

import List exposing (map)
import Html exposing (section, article, a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper, current, fromTree, children, root, openAll)

--App is an immutable type, a tree of Symbols, with its product as children.
--As a zipper, an App is aware of its parent.

--This module provides de/serialization. (TODO).
--An App is the result of a remix. (TODO).
--Each App streams its own, mutable collection of Receiver data. (TODO).


type alias App
  = Zipper (Symbol)

type Symbol
    = Template
    | Many
    | Name String --can be a reference or a definition.
    | Generate G
    --Receiver
type G
    = Emblem --allegory+poem.
    | Paragraph --contents, to put in a paragraph block.
    | Link --generator (target).
    | Title --generator (string). String doubles as id.
    | Caption --generator (string). String doubles as id.
    | Text --generator (string). just text.

view : App -> Html msg
view a =
  section [class "App"]
    [ h2 [] [text "App Structure"]
    , let
        captionProperties l = case children l of
          (x::y::xs) ->  [class "productCaption"]
          _          ->  [class ""]
        properties l = case children l of
          (x::y::xs) ->  [class "product"]
          _          ->  [class "app"]
        viewLevel l = div (captionProperties l) [viewSymbol (current l), div (properties l) <| List.map (viewLevel) (openAll l) ]
      in
        viewLevel (root a)
    ]

viewDescription app =
  case current app of
    Template -> "(Template)"
    Many -> "(Many)"
    Name s -> s
    Generate g -> case g of
       Emblem       -> "Emblem"
       Paragraph    -> "Paragraph"
       Link         -> "Link"
       Title        -> "Title"
       Caption      -> "Caption"
       Text         -> "Text"

viewSymbol symbol =
 let
  viewContainer    = div [class "app"]
  viewType         = span [class "type"]
 in case symbol of
  Template        -> viewContainer [viewType [text " < "] ]
  Many            -> viewContainer [viewType [text " + "] ]
  Name    s       -> viewContainer [viewType [text ("'"++s++"'")] ]
  Generate g -> case g of
     Emblem       -> viewContainer [viewType [text "Emblem"] ]
     Paragraph    -> viewContainer [viewType [text "Paragraph"] ]
     Link         -> viewContainer [viewType [text "Link"] ]
     Title        -> viewContainer [viewType [text "Title"] ]
     Caption      -> viewContainer [viewType [text "Caption"] ]
     Text         -> viewContainer [viewType [text "Text"] ]

type Data = Data Symbol (List Data)

initial : App
initial =
  let
      getChildren  (Data i c) = c
      getSymbol    (Data i c) = i
  in
    Data (Name "flupsicom")
            [ Data Many
              [ Data (Name "topic")
                [ Data (Generate Title) []
                , Data Many [ Data (Generate Caption) [] ]
                , Data Many [ Data (Generate Emblem)
                                   [ Data Many [ Data (Generate Paragraph) [ Data (Generate Text) [] ] ] ] ]
                , Data Many [ Data (Generate Paragraph)
                              [ Data Many [ Data (Generate Text) [] ]
                              , Data Many [ Data (Generate Link) [] ]
                              ] ] ] ] ]
    |> Tree.build getChildren  |> Tree.map getSymbol |> Zipper.fromTree
