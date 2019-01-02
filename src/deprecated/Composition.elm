module Composition exposing (Composition, edit, editInto, Item (..), Alternative (..), Template (..),
  reduceI, reduceA, reduceC, reduceM, increment, mapDescendants,
  getItem, setItem,
  append,
  zip,
  Input (..), Anchor (..), Media (..))

import App exposing (App, Symbol (..), G)
import Layout exposing (Layout (..))
import Visibility exposing (Visibility (..))

import Html exposing (a, h1, h2, h3, p, button, span, section, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class, href)
import Html.Events exposing (onClick)
import Lazy.Tree as Tree exposing (Tree(..), singleton)
import Lazy.Tree.Zipper as Zipper exposing (Zipper(..), updateItem, up, insert, getTree, open, current, children, root, openAll)
import List exposing (map, foldl)


--A Composition is Items in a Zipper.
--Each Item has a zipper with its edits. (local undo/redo for free)
--In relation to its app, the Composition omits all items that are not editable.


type alias Composition = Zipper Item

-- Item types --
type Item

  = Link (Zipper (Edit Anchor))
  | Text Role (Zipper (Edit Input))
  | Figure Layout (Zipper (Edit Media))
  -- Placeholder for an unspecified item.
  -- Selecting one of the alternatives replaces it with a fresh App.
  -- It has a visibility, so it can be dismissed.
  | Ambiguous (Visibility) (Alternative Template) (Zipper (Maybe Template))
  -- Wrappers (omitting < for now):
  -- Plus t n i grows its children by n instanciations of t.
  | Plus (Alternative Template) (Zipper (Edit Addition)) Item
  | Labeled String Item
  -- Empty Items:
  | Box                     -- Any relevant selection & navigation target, such as Paragraph, Chapter or Emblem
  | Leaf                    -- Needs to be rendered if the App ends with a container type such as Name, Many or Template.

-- A template is the relevant tree, with a focus on the parent Item.
type Template = Template App
type Alternative a = One a | More a (Alternative a) -- just a nonempty list
type Role = Title | Caption | Span







getItem : Zipper a ->  a
getItem = current

-- replace the current, but keep the descendants.
setItem : Item -> Composition -> Composition
setItem i = updateItem (always i)

-- replace the current and its tree all the way down of C0 with that of C1.
append : Composition -> Composition -> Composition
append = insert << getTree

--- Creation ---

zip            : a -> Zipper a
zip            = singleton  >> Zipper.fromTree

mapDescendants    : ( Zipper a -> b ) -> Zipper a -> List b
mapDescendants fu = ( map fu ) << Zipper.openAll

edit           : App -> Composition
edit app       = zip Box |> editInto app


--updates a composition to comply to an app.
--only goes down from the current focus.
editInto : App -> Composition -> Composition
editInto app composition =
  -- strategy: evaluate the current symbol,
  -- build appropriate item or affect parent item.
  -- if appropriate, grow children (via recursion).
  let
    subtypes : List App
    subtypes = app |> openAll

    -- go up one level in the composition.
    parent : Composition -> Composition
    parent c = c |> up |> Maybe.withDefault ((zip Box) |> (append c))

    -- go down 'from' to reach 'to'. If it's lost, just return 'to'.
    child : Composition -> Composition -> Composition
    child to from = from |> open ( (==) (current to) ) |> Maybe.withDefault to

    -- replace parent's item
    setParent : Item -> Composition -> Composition
    setParent i = updateParent (always i)

    -- update the item of the parent composition and come back here.
    updateParent : (Item -> Item) -> Composition -> Composition
    updateParent itemFu c = parent c |> updateItem itemFu |> child c

    -- map a function to the composition we are currently editing into.
    here : (Composition -> Composition) -> Composition
    here = (\f -> composition |> f)

    -- make any item accept additions (+).
    addPlus : Template -> Item -> Item
    addPlus templateToAdd i =
      case i of
              Plus      (One t)                            (edits) item
                -> Plus (More templateToAdd (One t))       (edits) item
              Plus      (More t more)                      (edits) item
                -> Plus (More templateToAdd (More t more)) (edits) item
              item
                -> Plus (One templateToAdd)             (zip Zero) item

    -- put the item into *here*, then insert the edit for each subtype.
    continueDown : Item -> Composition
    continueDown
     =  setItem             -- Item -> Composition -> Composition
     >> here                -- (Composition -> Composition) -> Composition
     >> insertSubtypes      -- C0 -> C0 with edited descendants

    -- *edit* each *subtype* to insert below *here*.
    insertSubtypes : Composition -> Composition
    insertSubtypes c =
       subtypes        --> List App
       |> map edit     --> List Composition
       |> foldl append c

  in
    case current app of

      -- Many: Add its children to composition's parent. Don't continue downwards.
      Many
       ->subtypes |> map Template                          --> all subtypes as Templates.
            |> List.foldl (addPlus >> updateParent) composition  --> add each as optional Pluses.
        -- This is really just the box.
        -- Because we intend this to be a mapping from App to Composition,
        -- ignoring data.


      -- Name:
      Name s
        -> case subtypes of
            one::[] -- just one subtype, it is devoured.
              -> editInto one composition |> updateItem (\i -> Labeled s i)
            _ -- Many subtypes, they are wrapped in a box.
              -> Box |> Labeled s |> continueDown

      Generate g ->
        case g of
          App.Emblem
            -> BlankMedia |> zip |> Figure Emblematic |> continueDown

          App.Paragraph
            -> Box |> continueDown

          App.Link
            -> BlankAnchor |> zip |> Link |> continueDown

          App.Title
            -> BlankInput |> zip |> Title |> continueDown

          App.Caption
            -> BlankInput |> zip |> Caption |> continueDown

          App.Text
            -> BlankInput |> zip |> Span |> continueDown

      --To Do: implement template.
      App.Template
          -> Box |> continueDown
