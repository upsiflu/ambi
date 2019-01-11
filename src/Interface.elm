
module Interface exposing
    -- draw interfaces --
    ( Interface
    , view
    
    , ActionRing
    , InteractiveElement
    , Interactivity (..)
    , Relative (..)

    -- intercept keys to annotate views --
    , Indicator
    , asAnnotator )


import List exposing (map)

import Lazy.Tree as Tree
import Lazy.LList as LList
import Lazy.Tree.Zipper as Zipper
import Stack exposing ( top, push, toList )

import Html exposing ( a as anchor, button, span )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )
import Html.Extra exposing (static)

import Url exposing (Url)






{-
    
    Interface
    
    provides defaults and boundaries for SPAs.
    An interface is a Constant that is fed lazy lookup
    functions for any data that may change. So you have to
    be explicit about mutability.

    In the current implementation, prologue and epilogue are
    static HTML.


 -- keys and Items
 
    Every item is referred to by its key. A stack of keys can
    be reconstructed using the getPath function.

    An Item is its key.

 
-- Talking to an Interface

    All aspects of an interface are either immutable
    or functions. To draw items, provide functions that accept
    stacks of keys. You can manage your own dicts.

    The window and the focus will be evaluated on each view.


 -- Wiring

    In your init, you have to provide all typical navigation
    messages so that the Interface can put them into your Html.

    The navigation messages are conceptually separate from the
    application-specific "actions" that you build your buttons
    with, or the hrefs you can use in links.

 -}



type alias Interface key data route nav action msg =

    {
    -- reading a Url
    , router: route -> Url
    , windowKeys: route -> Stack key
    , intendedFocus: route -> List key
    , prologue: route -> Static

    -- items over keys respond to data.
    , drawPassiveItem: 
        Stack key -> data -> Static
    , drawInteractiveItem: 
        Stack key -> data -> ActionRing ( Stack key ) action
    , getChildKeys: 
        Stack key -> data -> List key
    
    -- Provide these Messages for internal navigation.
    , putFocus: Stack key -> nav
    , back: nav
    
    , epilogue: Static
    , meta: Static
    }



type alias Static = [ Html Never ]



type Item keys action
    = Interactive ( ActionRing keys action )
    | Passive Static

type alias ActionRing keys action =
    Ring ( InteractiveElement keys action )

type alias InteractiveElement route keys action =
    { interactivity: Interactivity route keys action
    , representation: Static }

type Interactivity route keys action
    = Button action
    | Link { target: route, description: String }
    | ChangeFocus ( Relative keys )

type Relative keys
    = Self keys
    | Cancel
    | Ok
    | Dismiss
    | Back




------------------------------------------------------------------------------

--The view of an interface only depends on the route (TODO: and the me).

view : Interface key route nav action msg -> route -> Html msg
view interface route =
    let
        -- Evaluate lazy variables
        window : Stack key
        window = route |> interface.windowKeys
        focus : List key
        focusSteps = route |> interface.intendedFocus

        viewPrologue = static interface.prologue
        viewEpilogue = static interface.epilogue
        viewMeta = static interface.meta

        -- Decompose the zipper
        childStacks : Stack key -> Stack key
        childStacks stack =
            interface.getChildKeys stack
            |> map ( \key -> push key stack )
    
        asPassiveItem : Stack key -> Item
        asPassiveItem = ( withNavigation drawPassiveItem ) >> Passive

        asInteractiveItem : Stack key -> data -> Item
        asInteractiveItem = drawInteractiveItem >> Interactive

        withNavigation : ( Stack key -> data -> Static ) -> Stack key ->
                         ( Stack key -> data -> Html nav )
        withNavigation draw =
            \k -> li [ interface.putFocus k |> onClick ] [ draw k |> map static ]
            

        viewWindow : Html msg
        viewWindow =
            window                                              -- top item
            |> Tree.build childStacks                           -- Tree
            |> Zipper.fromTree                                  -- Zipper ( keys )
            |> Zipper.openPath ( \s k -> s == top k ) steps     -- walk to focus
            |> distinguishFocus asInteractiveItem asPassiveItem -- evaluate item kind 
            |> root |> getTree |> viewTree                      -- fold to Html

        viewTree : Tree -> Html msg
        viewTree tree =
            Tree.descendants tree |> LL.map viewTree |> viewItem ( Tree.item tree )
        
        viewItem : Item -> ( LList Item ) -> Html msg
        viewItem item children =
            let current =
                case item of
                    Passive content -> static content
                    Interactive actionRing ->
                        ringList actionRing |> List.map viewElement
        
        viewElement : InteractiveElement route key action |> Html action 
        viewElement { interactivity, representation } =
            case interactivity of
                Button action ->
                    button
                        [ onClick action ] representation
                Link target description ->
                    anchor
                        [ href ( interface.router target ) ]
                        [ text description ]
                ChangeFocus relation ->
                    case relation of
                        Self k ->
                            anchor
                                [ class "self", interface.putFocus k |> href ]
                                [ text "#" ]
                        Cancel ->
                            anchor
                                [ class "cancel", interface.back |> href ]
                                [ text "cancel" ]
                        OK ->       
                            anchor
                                [ class "ok", interface.back |> href ]
                                [ text "OK" ]
                        Dismiss ->
                            anchor
                                [ class "dismiss", interface.back |> href ]
                                [ text "v" ]
                        Back ->
                            anchor
                                [ class "back", interface.back |> href ]
                                [ text "<" ]

    in
        viewPrologue :: viewWindow :: viewEpilogue :: viewMeta :: []















-- Exposed functions

type alias Indicator keys = keys -> String

asAnnotator : Indicator keys -> ( keys -> Static ) -> ( keys -> Static )
asAnnotator indicator drawer
    = ( \key -> [ span [ class ( indicator keys ) ] ( drawer keys ) ] )
    



------------ STRUCTURES -------------------------------------------------------

-- Type helpers

distinguishFocus : ( i -> a ) -> ( i -> a ) -> Zipper i -> Zipper a
distinguishFocus focusfu nofu zipper =
    let newFocusedItem = Zipper.current zipper |> focusfu
    in Zipper.map nofu zipper |> Zipper.updateItem ( always newFocusedItem )


-- Ring is for Tabstops --


type alias Ring a =
    { position: Int,
    , slots: Array a
    }


ring : a -> List a -> Ring a
ring primary others =
    Array.fromList primary::others |> Ring 0

ringIsPrimary : Ring a -> Bool
ringIsPrimary r = r.position  == 0

ringPrimary r = { r | position = 0 }

ringNext : Ring a -> Ring a
ringNext r =
    { r | position = r.position + 1 |> modBy ( length r.slots )  }

ringPrev : Ring a -> Ring a
ringPrev r =
    { r | position = r.position - 1 |> modBy ( length r.slots ) }

ringCurrent : Ring a -> a
ringCurrent r =
    get r.position r.slots

ringList : Ring a -> List b
ringList r =
    Array.toList r.slots

ringMap : ( a -> b ) -> Ring a -> Ring b
ringMap fu a =
