
module UI exposing
    -- draw uis --
    ( UI
    , view
    
    , ActionRing
    , InteractiveElement
    , Interactivity (..)
    , Relative (..)
    , ring
    , Stack
    , Static 
    , create

    -- intercept stack to annotate views --
    , Indicator
    , asAnnotator )

import Debug

import Tuple exposing (first, second, mapFirst, mapSecond)
import List exposing (map)

import Lazy.Tree as Tree exposing ( Tree )
import Lazy.LList as LL exposing ( LList )
import Lazy.Tree.Zipper as Zipper exposing ( Zipper, root, getTree )

import Html exposing ( Html, text, a, button, span, li, ul, text, section )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )
import Html.Extra exposing ( static )

import Result exposing ( Result )

import Array exposing ( Array, length )
import Url exposing (Url)

import Tagged exposing ( tag )




{-
    
    UI
    
    provides defaults and boundaries for SPAs.
    An ui is a Constant that is fed lazy lookup
    functions for any data that may change. So you have to
    be explicit about mutability.

    In the current implementation, prologue and epilogue are
    static HTML.


 -- Semantic Markup

    Html5 introduces semantic elements with precise roles.
    UI provides Roles to markup individual items. Roles do
    not directly translate into Html Elements but are
    interpreted based on context. So depending on where a
    'Title' appears, it can be the h1 of the document, the
    h2 of an article or a h3.. of a nested article.
    Additionally, the role of an item may dictate its
    order and nesting, as in introductions being merged and
    enclosed, with title, into a header:

    -- article ---------
     -- header --------
         title
         introductions
         (autonav)
     ------------------
     -- main ----------
         miscs,
         lists
         (in any order)
     ------------------
     -- footer --------
         (autometa)
     ------------------
    --------------------
    


 -- stack and Items
 
    Every item is referred to by its key. A stack of stack can
    be reconstructed using the getPath function.

    An Item is its key.

 
-- Talking to an UI

    All aspects of an ui are either immutable
    or functions. To draw items, provide functions that accept
    stacks of stack. You can manage your own dicts.

    The window and the focus will be evaluated on each view.


 -- Wiring

    In your init, you have to provide all typical navigation
    messages so that the UI can put them into your Html.

    The navigation messages are conceptually separate from the
    application-specific "actions" that you build your buttons
    with, or the hrefs you can use in links.

 -}

type alias Static = List ( Html Never )
type alias Wrapper msg = List ( Html msg ) -> Html msg 

-- get a permanent link href to an item.
type alias Router =
    String -> String

-- get breadcrumbs to an item.
type alias Keys key state =
    state -> Stack key

-- get multiple single steps.
type alias Steps key state =
    state -> List key

-- get a static representation.
type alias Drawer state =
    state -> Static

-- get a static representation
-- as well as a wrapper for children
-- for an item
type alias KeyedDrawer key state msg =
    Stack key -> state ->
    ( Static, Maybe ( Wrapper msg ) )

-- get a static representation
-- as well as a wrapper for children
-- for an item
type alias KeyedInteractivator key state msg =
    Stack key -> state ->
    ( ActionRing ( Stack key ) msg, Maybe ( Wrapper msg ) )

type alias ChildSteps key state =
    Stack key -> state -> List key

type alias Messager key msg =
    Stack key -> msg

type alias Stringer key =
    Stack key -> String

type alias Message msg =
    msg

type alias Statically =
    Static

type alias UI key state msg =
 
    {
    -- reading a Url
      router: Router
    , windowKeys: Keys key state
    , intendedFocus: Steps key state
    , prologue: Drawer state

    -- items over stack respond to state.
    , drawPassiveItem: KeyedDrawer key state msg
    , drawInteractiveItem: KeyedInteractivator key state msg
    , getChildKeys: ChildSteps key state
    
    -- Provide these Messages for internal navigation.
    , putFocus: Messager key msg
    , getLink: Stringer key
    , back: Message msg
    
    , epilogue: Statically
    , meta: Statically
    }

type alias Readers key state =
    {
    -- reading a Url
      router: Router
    , windowKeys: Keys key state
    , intendedFocus: Steps key state
    , prologue: Drawer state
    }
type alias Drawers key state msg =
    {
    -- items over stack respond to state.
      drawPassiveItem: KeyedDrawer key state msg
    , drawInteractiveItem: KeyedInteractivator key state msg
    , getChildKeys: ChildSteps key state
    }
type alias Messagers key msg =
    {
    -- Provide these Messages for internal navigation.
      putFocus: Messager key msg
    , getLink: Stringer key
    , back: Message msg
    
    , epilogue: Statically
    , meta: Statically
    }


create : Readers key state -> Drawers key state msg -> Messagers key msg -> UI key state msg
create readers drawers messagers =
    {
    -- reading a Url
      router = readers.router
    , windowKeys = readers.windowKeys
    , intendedFocus = readers.intendedFocus
    , prologue = readers.prologue

    -- items over stack respond to state.
    , drawPassiveItem = drawers.drawPassiveItem
    , drawInteractiveItem = drawers.drawInteractiveItem
    , getChildKeys = drawers.getChildKeys
    
    -- Provide these Messages for internal navigation.
    , putFocus = messagers.putFocus
    , getLink = messagers.getLink
    , back = messagers.back
    
    , epilogue = messagers.epilogue
    , meta = messagers.meta
    }




-- An element that is already assigned a position (stack).
type Item state stack msg
    = Interactive ( state -> ( ActionRing stack msg ) )
    | Passive ( state -> List ( Html msg ) ) -- forgets its stack

-- Rotate with Tab key.
type alias ActionRing stack msg =
    Ring ( InteractiveElement stack msg )

-- Define an element as the product of one interactivity and one representation.
type alias InteractiveElement stack msg =
    { interactivity: Interactivity stack msg
    , representation: Static }

type Interactivity stack msg
    = Button msg
    | Link { target: String, description: String }
    | ChangeFocus ( Relative stack )

type Relative stack
    = Self stack
    | Cancel
    | OK
    | Dismiss
    | Back




------------------------------------------------------------------------------

--The view of an ui only depends on the state.
view : state -> UI key state msg -> Html msg
view state ui =
    let
        -- Evaluate lazy variables
        window : Stack key
        window = state |> ui.windowKeys
        steps : List key
        steps = state |> ui.intendedFocus

        -- Draw static Html
        viewPrologue = List.map static ( ui.prologue state ) |> section []
        viewEpilogue = List.map static ui.epilogue |> section []
        viewMeta = List.map static ui.meta |> section []

        -- Append each child keys to a copy of this stack
        childStacks : Stack key -> List ( Stack key )
        childStacks stack =
            ui.getChildKeys stack state
            |> map ( \key -> push key stack )
    
        -- Any passive item doubles as a navigation button to itself
        toPassiveItem : Stack key -> Item state ( Stack key ) msg
        toPassiveItem stack =
            ui.drawPassiveItem stack
            |> mapFirst withNavigation
            |> Passive
                  
        -- ActionRings are simply wrapped in an Item container
        toInteractiveItem : Stack key -> Item state ( Stack key ) msg
        toInteractiveItem =
            ui.drawInteractiveItem >> Interactive
  
        -- puts anything in a link leading to itself
        withNavigation : ( Stack key -> state -> List ( Html Never ) ) -> 
                         ( Stack key -> state -> List ( Html msg ) )
        withNavigation drawer =
            let
                newDrawer : Stack key -> state -> List ( Html msg )
                newDrawer stack st =
                    drawer stack st                                  --draw
                    |> List.map static                              --make general
                    |> a [ href ( ui.getLink stack ), class "item" ]
                    |> \n -> n :: []
            in
                newDrawer
            
        -- build the tree from the window, focus, map to items.
        buildWindow : Zipper ( Item state ( Stack key ) msg )
        buildWindow =
            window                                              -- top item
            |> Tree.build childStacks                           -- Tree
            |> Zipper.fromTree                                  -- Zipper ( stack )
            |> Zipper.attemptOpenPath (
                    \s stack ->
                    let t = top stack
                    in case t of
                        Just k -> k == s
                        Nothing -> False
                ) steps     -- walk to focus
            |> distinguishFocus toInteractiveItem toPassiveItem -- evaluate item kind 
         

---------- VIEWING -----------------------------------------------------------

        -- new: we first map the drawFunction, then we take the first 100.

        viewWindow = buildWindow |> Zipper.map viewItem
 
        viewItems = zipperTake maximumRecursionDepth viewWindow |> ul []
        maximumRecursionDepth = 6

        zipperTake max zipper =
            let
                children = if ( max < 0 ) then [] else
                    Zipper.openAll zipper |> List.map ( zipperTake ( max - 1 ) )
                list = List.foldl ( \child acc -> child ++ acc ) [] children
                
            in
                ( Zipper.current zipper ) ++ ([ span [ class "depth" ][ text ( String.fromInt max ) ]]) ++ list


        viewItem : Item state ( Stack key ) msg 
            -> List ( Html msg )
        viewItem item =
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
                    a   [ href ( ui.router target ), class "item" ] <|
                        List.map static representation ++ [ text description ]
                ChangeFocus relation ->
                    case relation of
                        Self k ->
                            a   [ class "self", ui.getLink k |> href ]
                                [ text "#" ]
                        Cancel ->
                            button   [ class "cancel", onClick ui.back ]
                                [ text "cancel" ]
                        OK ->       
                            button   [ class "ok", onClick ui.back ]
                                [ text "OK" ]
                        Dismiss ->
                            button   [ class "dismiss", onClick ui.back ]
                                [ text "v" ]
                        Back ->
                            button   [ class "back", onClick ui.back ]
                                [ text "<" ]

    in
        ul [] <| viewPrologue :: viewItems :: viewEpilogue :: viewMeta :: []















-- Exposed functions

type alias Indicator stack = stack -> String

asAnnotator : Indicator stack -> ( stack -> Static ) -> ( stack -> Static )
asAnnotator indicator drawer
    = ( \stack -> [ span [ class ( indicator stack ) ] ( drawer stack ) ] )
    



------------ STRUCTURES -------------------------------------------------------

type alias Stack a = List a

push : a -> Stack a -> Stack a
push k stack =
    k::stack

pop : Stack a -> Stack a
pop stack =
    case stack of
        s::tack -> tack
        [] -> []

top : Stack a -> Maybe a
top stack = 
    case stack of
        s::tack -> Just s
        [] -> Nothing
        
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

