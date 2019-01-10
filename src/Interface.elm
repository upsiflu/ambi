module Interface exposing ( Skeleton, Location (..) )


import Html exposing ( a as anchor, button, span )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )

-- This module provides a limited structure for SPAs.
-- It only accepts passive view functions so you have to specify
-- any interactivity through Interface's presets.
-- This module contains no state. The browser is handling the tabstop thing, I guess...
-- That is true if we decide to never change the tabIndex of anything.















type Skeleton key msg
    = Skeleton
        { prologue: Prologue
        , items: Items k msg
        , epilogue: Epilogue
        , meta: Meta }





type Interface
    = D Drawable
    | I Interactive

type alias Drawable = Prologue Items Epilogue Meta Drawer
type alias Interactive = Interactive Prologue Items Epilogue Meta Drawer Navigator Interactor




type alias Prologue = Html Never
type alias Items k msg = Zipper ( Item k msg )
type alias Epilogue = Html Never
type alias Meta = { closed: Html Never }


type alias Drawer k = k -> List (Html Never)
type Interactor k msg = Interactor (k -> List ( Html msg ))
type alias Navigator msg =
                { dismiss: msg
                , focus: msg
                , blur: msg
                , meta: msg
                , context: msg }

type alias Indicator = key -> String


type alias Item k msg =
    --{ controls: Ring ( { action: Action msg, drawer: Drawer k } )}

type Action k a
    = Button a
    | Link { target: k, description: String }
    | Control (UI k)

type UI k
    = Self k
    | Cancel k
    | Ok k
    | Dismiss k
    | Back





-------------------- Exposed functions ----------------------------------

asAnnotator : Indicator -> Drawer -> Drawer
asAnnotator indicator drawer
    = ( \key -> [ span [ class ( indicator key ) ] ( drawer key ) ] )
    


---------------------- Populate an Interface ----------------------------

draw : Skeleton k msg -> Drawer -> Interface k msg
draw (Skeleton base) dra =
    D { base | drawer = dra }

wire : Interface k msg -> Navigator k msg -> Interactor k msg -> Interface k msg
wire i nav int = 
    case i of
        D drawable ->
            I { drawable | navigator = nav, interactor = int }
        I interactive -> I
            { interactive | navigator = nav, interactor = int }

view : Interface k msg -> Html msg
view i =

    let

        ----------- LAYOUT -----------------------------------

        drawPage =
            biPrepend i.prologue i.entries
                |> biFlattenFocus drawEntryFocus drawEntry

        drawEpilogue =
            i.epilogue.view mode

        drawMeta =
            case i.meta of
                Nothing |> []
                _ |> []


        ------------ VIEWS -----------------------------------

        drawFocus focused = 
            ringList focused.tabstops |> List.map drawTabstop

        drawEntry entry =
            -- todo: context menu etc
            ringPrimary entry.tabstops |> ringCurrent |> drawTabstop 

        drawTabstop
            { primaryAction
            , contextAction
            , view
            , secondaryZipper
            } =
                case primaryAction of
                    
                    Button msg ->
                        button [ onClick msg ] [ view mode ]
                    Link target description ->
                        anchor [ href target ] [ description ]
                    Control ui ->
                        case ui of
                            Self ->   anchor [ class "self", i.self |> href ] [ view mode ]
                            Cancel -> anchor [ class "cancel", i.cancel |> withDefault i.leave |> href ] [ view mode ]
                            OK ->     anchor [ class "ok", i.leave |> withDefault i.back |> href ] [ text "OK" ]
                            Back ->   anchor [ class "back", i.back |> withDefault i.leave |> href ] [ text "<" ]

    in
        drawPage ++ drawEpilogue ++ drawMeta




------------ STRUCTURES -------------------------------------------------------











-- Binary Zipper is for arrow key navigation.

type alias BiZipper a =
    { up: List (Zipper a)
    , horizontal: Zipper a
    , down: List (Zipper a)
    }

type alias Zipper a =
    { left: List a
    , current: a,
    , right: List a
    }

down b =
    case b.down of
        []    -> b
        [dow::n] ->
            { up = b.horizontal :: b.up
            , horizontal = dow
            , down = n
            }

up b =
    case b.up of
        []    -> b
        [u::p] ->
            { up = p
            , horizontal = u
            , down = b.horizontal :: b.down
            }

current b =
    b.horizontal.current

mapHorizontal fu h = mapHorizontalFocus fu fu h

mapHorizontalFocus focusFu fu h =
    ( List.map fu h.left )   ++ 
    [ focusFu <| h.current ] ++
    ( List.map fu h.right )

biPrepend : Zipper a -> BiZipper a -> BiZipper a
biPrepend pre::pend =
    { pend | up = pend.up ++ [pre] }

biFlattenFocus : (a -> b) -> (a -> b) -> BiZipper a -> List b
biFlattenFocus biz focusFu fu =
    ( List.map (mapHorizontal fu) biz.up ) ++
    ( mapHorizontalFocus focusFu fu biz.horizontal ) ++ 
    ( List.map (mapHorizontal fu) biz.down )





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
