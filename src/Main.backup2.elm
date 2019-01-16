
port module Main exposing (..)

import Browser.Navigation as Navigation
import Url exposing (Url)
import Browser


import Stack exposing ( Stack, push, pop )
import Dict exposing ( Dict )

import UI exposing ( UI, view, Interactivity (..), ring )

import Html exposing ( text )











{-
    
    Main
    
  * curator shadows the auth, user and avatar provided by the js system
  * route shadows the state of the URL bar
  * site shadows the database document for the given site
  * local knows which data is in sync between the local js and the database
  * ui is the immutable configuration (wiring) between data and view
  
 -}
    

type alias Model =
    { data: Dict Int String
    , ui: UI ( Stack Int ) String Msg 
    , key: Navigation.Key
    }



init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        fromList =
            List.foldl ( \k acc -> Stack.push k acc ) Stack.initialise
        initialData =
            Dict.fromList
                [ (fromList [1], "Alice")
                , (fromList [1, 1], "Bob")
                , (fromList [1, 2], "Chuck")
                , (fromList [1, 1, 1], "Duck")
                ]

        theChildren =
            Dict.fromList
                [ (fromList [1], [1, 2])
                , (fromList [1, 1], [1])
                , (fromList [1, 2], [])
                , (fromList [1, 1, 1], [])
                ]

        getChildSteps k = Dict.get k theChildren

        initialUI =

            { prologue = [ text "prologue" ]
            , epilogue = [ text "epilogue" ]
            , meta = [ text "meta" ]
            --------------------------------------------------
            , drawPassiveItem = \_ _ ->
                    [ text "inactive" ]
            , drawInteractiveItem = \_ _ ->
                UI.ring 
                    { interactivity= Link { target = "", description = "test" }
                    , representation= [ text "FOCUS" ] } []
            , getChildKeys = getChildSteps
            , router = identity -- for strings
            --------------------------------------------------
            , putFocus = PutFocus
            , back = Back
            --------------------------------------------------
            , windowKeys = 0
            , intendedFocus = 5
            }

    in
        (
            { data = initialData
            , ui = initialUI
            , key = key
            }
            , Cmd.none
        )


        
---- UPDATE ----


type Msg

{-
    --- Database
    = Incoming Value 
    | Outgoing Value
-}
    --- Routing
    = UrlChanged Url
    | LinkFollowed Browser.UrlRequest

{-

    --- Editing
    | Input String Locus
    | Append Locus
    | Remove Locus
    | Discuss Locus
    | Decide Locus
-}

    --- UI
    | PutFocus String
    | Back

{-
port incoming : ( Value -> msg ) -> Sub msg
port outgoing :   Value          -> Cmd msg
-}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --Incoming v -> incoming v
        --Outgoing v -> outgoing v
        LinkFollowed urlRequest ->
            case urlRequest of
                Browser.Internal url -> (model, Navigation.pushUrl model.key)
                Browser.External href -> (model, Navigation.load href)
        UrlChanged new -> ( model, Cmd.none )


        
    



---- VIEW ----


view { data, ui, key } =
    let state = data
    in
    { title = "tangible 0.0.9"
    , body = ui.view state
    }






    {--
subscriptions : Model -> Sub Msg
subscriptions model = incoming Incoming
--}


---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    , onUrlRequest = LinkFollowed
    , onUrlChange  = UrlChanged
    }























