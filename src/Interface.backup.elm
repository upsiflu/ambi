
module Interface exposing
    -- draw interfaces --
    ( Interface
    , view
    
    , ActionRing
    , InteractiveElement
    , Interactivity (..)
    , Relative (..)
    , ring

    -- intercept keys to annotate views --
    , Indicator
    , asAnnotator )


import List exposing (map)

import Lazy.Tree as Tree exposing ( Tree )
import Lazy.LList as LL exposing ( LList )
import Lazy.Tree.Zipper as Zipper exposing ( Zipper, root, getTree )
import Stack exposing ( top, push, toList )

import Html exposing ( Html, text, a, button, span, li, ul, text, section )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )
import Html.Extra exposing ( static )

import Result exposing ( Result )

import Array exposing ( Array, length )
import Url exposing (Url)

import Stack exposing ( Stack, top, push )




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



type alias Interface key state msg =

    {
    -- reading a Url
      router: String -> String
    , windowKeys: state -> Stack key
    , intendedFocus: state -> List key
    , prologue: state -> Static

    -- items over keys respond to state.
    , drawPassiveItem: 
        Stack key -> state -> Static
    , drawInteractiveItem:
        Stack key -> state -> ActionRing ( Stack key ) msg
    , getChildKeys:
        Stack key -> state -> List key
    
    -- Provide these Messages for internal navigation.
    , putFocus: Stack key -> msg
    , getLink: Stack key -> String
    , back: String
    
    , epilogue: Static
    , meta: Static
    }



type alias Static = List ( Html Never )


-- An element that is already assigned a position (keys).
type Item state keys msg
    = Interactive ( state -> ( ActionRing keys msg ) )
    | Passive ( state -> List ( Html msg ) )

type alias ActionRing keys msg =
    Ring ( InteractiveElement keys msg )

type alias InteractiveElement keys msg =
    { interactivity: Interactivity keys msg
    , representation: Static }

type Interactivity keys msg
    = Button msg
    | Link { target: String, description: String }
    | ChangeFocus ( Relative keys )

type Relative keys
    = Self keys
    | Cancel
    | OK
    | Dismiss
    | Back




------------------------------------------------------------------------------

--The view of an interface only depends on the state.

view : Interface key state msg -> state -> Html msg
view interface state =
    let
        -- Evaluate lazy variables
        window : Stack key
        window = state |> interface.windowKeys
        steps : List key
        steps = state |> interface.intendedFocus

        viewPrologue = List.map static ( interface.prologue state ) |> section []
        viewEpilogue = List.map static interface.epilogue |> section []
        viewMeta = List.map static interface.meta |> section []

        -- Decompose the zipper
        childStacks : Stack key -> List ( Stack key )
        childStacks stack =
            interface.getChildKeys stack state
            |> map ( \key -> push key stack )
    
        toPassiveItem : Stack key -> Item state ( Stack key ) msg
        toPassiveItem keys =
            ( withNavigation interface.drawPassiveItem ) keys
            |> Passive
                
        toInteractiveItem : Stack key -> Item state ( Stack key ) msg
        toInteractiveItem = interface.drawInteractiveItem >> Interactive

        withNavigation : ( Stack key -> state -> List ( Html Never ) ) -> 
                         ( Stack key -> state -> List ( Html msg ) )
        withNavigation drawer =
            let
                newDrawer : Stack key -> state -> List ( Html msg )
                newDrawer keys st =
                    drawer keys st                                  --draw
                    |> List.map static                              --make general
                    |> button [ interface.putFocus keys |> onClick ]
                    |> \n -> n :: []
            in
                newDrawer
            

        viewWindow : Html msg
        viewWindow =
            window                                              -- top item
            |> Tree.build childStacks                           -- Tree
            |> Zipper.fromTree                                  -- Zipper ( keys )
            |> Zipper.attemptOpenPath ( 
                    \s keys ->
                    let t = top keys
                    in case t of
                        Just k -> k == s
                        Nothing -> False
                ) steps     -- walk to focus
            |> distinguishFocus toInteractiveItem toPassiveItem -- evaluate item kind 
            |> root >> getTree
            |> viewTree    -- fold to Html
            |> ul []

        viewTree : Tree ( Item state ( Stack key ) msg )
                -> List ( Html msg )
        viewTree tree =
            Tree.descendants tree                       -- LList Tree
            |> LL.map viewTree                          -- LList Tree
            |> viewItem ( Tree.item tree )
        
        viewItem : Item state ( Stack key ) msg 
            -> LList ( List ( Html msg ) )
            -> List ( Html msg )
        viewItem item children =
            case item of
                    Passive content -> content state
                    Interactive actionRing ->
                        ringList ( state |> actionRing )
                        |> List.map viewElement
        
        viewElement : InteractiveElement ( Stack key ) msg -> Html msg 
        viewElement { interactivity, representation } =
            case interactivity of
                Button msg ->
                    button
                        [ onClick msg ] <| List.map static representation
                Link { target, description } ->
                    a   [ href ( interface.router target ) ]
                        [ text description ]
                ChangeFocus relation ->
                    case relation of
                        Self k ->
                            a   [ class "self", interface.getLink k |> href ]
                                [ text "#" ]
                        Cancel ->
                            a   [ class "cancel", interface.back |> href ]
                                [ text "cancel" ]
                        OK ->       
                            a   [ class "ok", interface.back |> href ]
                                [ text "OK" ]
                        Dismiss ->
                            a   [ class "dismiss", interface.back |> href ]
                                [ text "v" ]
                        Back ->
                            a   [ class "back", interface.back |> href ]
                                [ text "<" ]

    in
        ul [] <| viewPrologue :: viewWindow :: viewEpilogue :: viewMeta :: []















-- Exposed functions

type alias Indicator keys = keys -> String

asAnnotator : Indicator keys -> ( keys -> Static ) -> ( keys -> Static )
asAnnotator indicator drawer
    = ( \keys -> [ span [ class ( indicator keys ) ] ( drawer keys ) ] )
    



------------ STRUCTURES -------------------------------------------------------

-- Type helpers

distinguishFocus : ( i -> a ) -> ( i -> a ) -> Zipper i -> Zipper a
distinguishFocus focusfu nofu zipper =
    let newFocusedItem = Zipper.current zipper |> focusfu
    in Zipper.map nofu zipper |> Zipper.updateItem ( always newFocusedItem )


-- Ring is for Tabstops --


type alias Ring a =
    { position: Int
    , slots: Array a
    }


ring : a -> List a -> Ring a
ring primary others =
    { position= 0, slots= Array.fromList <| primary::others }

ringIsPrimary : Ring a -> Bool
ringIsPrimary r = r.position  == 0

ringPrimary r = { r | position = 0 }

ringNext : Ring a -> Ring a
ringNext r =
    { r | position = r.position + 1 |> modBy ( length r.slots )  }

ringPrev : Ring a -> Ring a
ringPrev r =
    { r | position = r.position - 1 |> modBy ( length r.slots ) }

ringCurrent : Ring a -> Maybe a
ringCurrent r =
    Array.get r.position r.slots

ringList : Ring a -> List a
ringList r =
    Array.toList r.slots

