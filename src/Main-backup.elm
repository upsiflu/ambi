
port module Main exposing (..)

import Browser.Navigation as Navigation
import Url exposing (Url)

import Type exposing (Type)
import Me exposing (Me, nobody)
import Route exposing (visit, view)
import Json.Decode exposing (Value)

import Stack exposing ( push, pop )















{-
    
    Main
    
  * curator shadows the auth, user and avatar provided by the js system
  * route shadows the state of the URL bar
  * site shadows the database document for the given site
  * local knows which data is in sync between the local js and the database
  * interface is the immutable configuration (wiring) between data and view
  
 -}
    

type alias Model =
    { curator: Me
    , route: Route
    , site: Site
    , local: Local
    , interface: Interface
    , key: Navigation.Key
    }



init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( initialRoute, initialDatabaseRequest )
            = Route.visit url

        withSyncIndications : ( Locus -> Html Never ) -> ( Locus -> Html Never )
        withSyncIndications =
            ( Local.syncStateIndicator local ) |> Interface.asAnnotator


        initialInterface =

            { prologue = Route.viewPrologue
            , epilogue = Route.viewEpilogue 
            , meta = Me.viewMeta
            --------------------------------------------------
            , drawPassiveItem = Kind.drawPassiveItem >> withSyncIndications
            , drawInteractiveItem = Kind.drawInteractiveItem >> withSyncIndications
            , getChildKeys = App.getChildSteps
            , router = Route.toUrl
            --------------------------------------------------
            , putFocus = PutFocus
            , back = Back
            --------------------------------------------------
            , windowKeys = Route.window
            , intendedFocus = Route.focus
            }

    in
        (
            { curator = Me.nobody
            , route = initialRoute
            , site = Site.loading route.location 
            , local = Local.
            , interface = initialInterface
            , key = key
            }
            
            , initialDatabaseRequest 
        )


        
---- UPDATE ----


type Msg

    --- Database
    = Incoming Value 
    | Outgoing Value

    --- Routing
    | UrlChanged Url
    | LinkFollowed Browser.UrlRequest

    --- Editing
    | Input String Locus
    | Append Locus
    | Remove Locus
    | Discuss Locus
    | Decide Locus

    --- UI
    | PutFocus Locus
    | Back


port incoming : ( Value -> msg ) -> Sub msg
port outgoing :   Value          -> Cmd msg

update : Msg -> Model -> ( Value, Cmd Msg )
update msg model =
    case msg of
        Incoming v -> incoming v
        Outgoing v -> outgoing v
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url -> (model, Navigation.pushUrl model.key)
                Browser.External href -> (model, Navigation.load href)
        UrlChanged new -> ( { model | route = Route.visit new }, Cmd.none )


        
    



---- VIEW ----


view { me, route, site, local, interface, key } =
    { title = "tangible 0.0.9"
    , body = interface.view route
    }






    
subscriptions : Model -> Sub Msg
subscriptions model = incoming Incoming



---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = RequestedNavigation
    , onUrlChange  = UrlChanged
    }























