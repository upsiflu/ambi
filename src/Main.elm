module Main exposing (..)

--import Composition exposing (Composition)
--import Site exposing (Site, fromApp)
import App exposing (initial, App)
import Composition exposing (Composition, edit, editInto, Item (..), Alternative (..), Template (..),
              reduceI, reduceA, reduceC, reduceM, increment, mapDescendants,
              getItem, setItem,
              append, zip,
              Input (..), Anchor (..), Media (..))
import Layout exposing (Layout (..))
import Visibility exposing (Visibility (..))

import Tuple exposing (pair, second)
import Browser
import Html exposing (a, h1, h2, h3, p, button, section, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class, id, href)
import Html.Events exposing (onClick)

{--
General Layout:

A Session is either visiting a Site,
or editing a Composition,
or reviewing an App.

--}

---- MODEL ----
type Model
   = {route:Route, author:Me}

type Version        = Version {number:Int, edits:List Edit}
type Site           = Site  {app:App, version:Version}
type RouteMode      = Review Version | Default

-- Route example: tangiblemodel.com/ava/blog/?review=draft#post.0/section.1/picture.3
type Route          = Route {site:Site, window:Locus, mode:RouteMode, focus:Locus}
type Avatar         = Novatar | Avatar String
type Me             = Nobody | Creating Auth Avatar | Switching Auth Avatar | Embodying Auth Avatar

init : ( Model, Cmd Msg )
init = ( App.initial-- |> Composition.edit |> Editing, Cmd.none)



---- UPDATE ----


type Msg
    = Clicked Item
    | Increment ...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    update msg model =
      --the update composes a composition.
      case Model of
        Visiting r
         -> (site, focus, app, version, session, composition) = (r.site, r.focus,
        Editing r me
         ->
      in case msg of
        Propose edit
          -> if (auth == Nothing) then model else
          -- send to DB and indicate proposal through an animation
        Successful edit
          -> -- indicate that it has merged through an animation
        Trigger locus
          -> let item = locus |> getItem in
              -- change focus and perhaps switch a variable in the item
        MakeSuggestions locus
          ->
        MakeAssertions locus
          ->
        OpenReview
          ->
        OpenAvatar
        SwitchAvatar new
        CreateAvatar



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Text" ]
        , case model of
           --Visiting site -> Site.view site
           Editing composition ->
             section [ class "Composition" ]
                     [ h2 [] [text "Composition"]
                     , composition |> viewComposition ]
           --Reviewing app -> App.view app
        ]

type Attachment
   = Label String
   | PickOne {options:Alternative Template, cancel:Msg}
   | Action Msg
   | NoAttachment

viewComposition : Composition -> Html Msg
viewComposition composition =
  let
    -- rendering the children
    viewChildren = composition |> mapDescendants viewComposition
    noChildren = viewChildren == []

    -- apply a layout and place the rendered children
    arrangeDown layout element =
      let
        viewFrame c = section [class c]
        viewChildrenSeparately = if noChildren then [] else [viewFrame "separately" viewChildren]
      in case layout of
        Flat         -> element::(text"flat...")::viewChildren           |> viewFrame "flat"
        Emblematic   -> element::viewChildrenSeparately |> viewFrame "emblematic"
        Illustrative -> element::viewChildrenSeparately |> viewFrame "illustrative"
        Captioned    -> element::(text"captioned...")::viewChildrenSeparately |> viewFrame "captioned"

    viewAttachment attachment =
      case attachment of
       ( PickOne { options, cancel } ) ->
         section [ id "sheet" ]
                 [ options |> presentTemplates, button [ onClick cancel ] [ text "v" ] ]
       ( Action action ) ->
         let
          (icon, descriptor) =
            case action of
              Modify {--c ->
                case getItem c of
                  Plus (One (Template t)) additions item ->
                    ("+", App.viewDescription t)
                  _ ->
                    ("-", "not an addition")c--} ->("?", "is a modification")
              _ ->
                ("?", "not a modification")
         in section [ id "action", onClick action ]
                    [ div [ class "actionIcon" ] [ text icon]
                    , div [ ] [ text descriptor ] ]
       _ -> text ""

    -- present a list of buttons that link alternative templates to instanciate
    presentTemplates alternative =
      let
        pickTemplate t = {--composition |> editInto t |>--} Disambiguate
        renderOption (Template t) = button [onClick (pickTemplate t)] [App.view t] --shows just the structure, no preview.
      in case alternative of
        One (Template t)
         -> li [] [renderOption (Template t)]
        More t tt
         -> ul [] ((renderOption t) :: [presentTemplates tt])


    viewItem : Item -> Attachment -> Html Msg
    viewItem item attachment =
      let
        viewLabel
         = span [class "label"] [
            text ( case attachment of
                      Label s
                       -> s
                      PickOne {options, cancel}
                       -> "Pick One"
                      Action     a
                       -> case a of
                          Navigate -> "Bar/Navigate"
                          Disambiguate -> "Bar/Disambiguate"
                          Modify -> "Bar/Modify"
                      _ -> ""
                  ) ]

        -- rendering the data --
        viewInput edits container =
          let input = case reduceI edits of
                        Input s -> s
                        BlankInput -> ""
          in  container [] [ viewLabel, text input]

        viewAnchor edits container =
          let anchor = case reduceA edits of
                        Anchor s -> "==> "++s
                        BlankAnchor -> ""
          in  container [href anchor] [viewLabel, text anchor]

        viewMedia edits container =
          let media = case reduceM edits of
                        Media s -> s
                        BlankMedia -> "missingImage.jpg"
          in  div [] [viewLabel, container [href media, src media] []]

      in case item of

        Link           edits -> viewAnchor edits a        |> arrangeDown Flat
        Title          edits -> viewInput  edits h2       |> arrangeDown Flat
        Caption        edits -> viewInput  edits h3       |> arrangeDown Captioned
        Span           edits -> viewInput  edits span     |> arrangeDown Flat
        Figure  layout edits -> viewMedia  edits img      |> arrangeDown layout

        -- In case of an ambiguity, we don't want to display the children.
        Ambiguous visibility alternative choices ->
          -- Choice postponed?
          case visibility of
            Actionable ->
              let
                postponeSheet
                 = {--composition
                    |> setItem (Ambiguous Postponed alternative choices)
                    |> --}Navigate
              -- Something chosen? We render the Composition with the chosen template.
              -- Nothing chosen? We present a Sheet with the available templates instead of other stuff.
              in
                case reduceC choices of
                  Just (Template app)
                   -> edit app
                      |> viewComposition
                  Nothing
                   -> PickOne { options=alternative, cancel=postponeSheet }
                      |> viewAttachment
            Postponed ->
              let
                reopenSheet
                 = {--composition
                    |> setItem (Ambiguous Actionable alternative choices)
                    |> --}Navigate
              in
                section [ class "postponedSheet", onClick reopenSheet ]
                        [ viewLabel, text ": ?" ]

        -- In case of an item that can add new children, we do want to display
        -- the children. And the + action will be the first child.
        Plus alternative additions innerItem ->
          let
            -- the theoretical state after a plus was pressed:
            plusAction : Composition -> Attachment
            plusAction c
             = Action <| Modify {--<|
                case alternative of

                 One (Template app)
                  -> edit app |> append c
                 options
                  -> Ambiguous Actionable options (zip Nothing) |> zip |> append c--}
          in
            composition
            |> setItem (Plus alternative (increment additions) innerItem) -- one more
            |> plusAction  -- is the action
            |> viewItem innerItem   -- as attachment to viewing the actual item
            |> arrangeDown Flat      -- and we continue with all the children

        Labeled name innerItem
         -> Label name
          |> viewItem innerItem

        Leaf -> span [] [viewLabel, text ": LEAF"]

        Box -> p [] <| (text "Box> ")::viewChildren

   in Label "Composition" |> viewItem (getItem composition)




---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
