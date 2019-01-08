module Interface exposing (Location(..))


import Html exposing ( a as anchor, button )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )

-- This module provides a limited structure for SPAs.
-- It only accepts passive view functions so you have to specify
-- any interactivity through Interface's presets.
-- This module contains no state. The browser is handling the tabstop thing, I guess...
-- That is true if we decide to never change the tabIndex of anything.















type Mode = Default -- later, we can add Enrichments and add variety to each viewer

-------------------------------------

-- k: key type, i.e. Locus. msg: action type --

type alias Interface k msg =
    { prologue: Zipper  ( Entry k msg )        -- first entry to items; horizontal movement is possible.
    , entries: BiZipper ( Entry k msg )        -- vertical and horizontal movement.
    , epilogue: { view: Viewer }
    , meta: Maybe ( Meta k msg )


    ---- Locations -----------------------



    ---- Messages ----------------------

    , switchMode: Mode -> msg
    , changeData: Changer
    }

type alias Viewer = Mode -> Html Never
type alias Changer k a msg = k -> a -> msg




type alias Meta k msg =
    { closed: Viewer
    , opened: Interface k msg }

type alias Entry msg =
    { escape: msg -- so that Escape can always go up one level and finally go to meta.
    , contextMenu: Maybe Mode -- switch mode on longtouch or rightclick or menukey. Prohibited on text.
    , tabstops: Ring ( Tabstop k msg )
    }


-- A tabstop is "one thing" in the interface. It must have at least a primary action and a viewer.
-- Switch viewer when you switch the enrichment. The viewer is where you can present data.
-- Escape is always the containing entry's escape.

type alias Tabstop msg
    { primaryAction: Action msg           -- onClick (and, by Browser, through other means)
    , contextAction: Maybe ( Action msg )
    , view: Viewer
    }

type Action k a
    = Button a
    | Link { target: k, description: String }
    | Control (UI k)

type UI k
    = Self k
    | Cancel k
    | Ok k
    | Back


draw : Interface k a msg -> Mode -> Html msg
draw i mode =

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
