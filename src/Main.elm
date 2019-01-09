port module Main exposing (..)

import Browser.Navigation as Navigation
import Url exposing (Url)

import Type exposing (Type)
import Me exposing (Me, nobody)
import Route exposing (visit, view)
import Json.Decode exposing (Value)

















{----------------------------------------------------------------
    
    Main
    
    generates a runtime.
    All state is encoded here:
    
  * Route shadows the state of the URL bar
  * Me ...?
  * Site shadows the database document for the given site

    Go here for view and update.
    
 ----------------------------------------------------------------}
    

type alias Model =
    { curator:Me
    , route:Route
    , site:Site
    , key:Navigation.Key
    }

init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( route, databaseRequest )
            = Route.visit url
    in
        (
            { route: route
            , curator: Me.nobody
            , site: Site.loading route.location 
            , key: key
            }
            , databaseRequest
        )

    
---- ERRORS ----

type Error
    = UrlError String
    | 
    
    
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

    --- UI
    | Discuss Locus
    | Dismiss Locus
    | Focus Locus
    | Blur Locus


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


view : Model -> Html Msg
view { me, route, site } =
    
    let
    --- for Accessibility
        interactionControls =
            Interface.interaction
                { dismiss: Dismiss
                , focus: Focus
                , blur: Blur
                , meta: Menu
                , context: Context }

    --- specific to this application
        editingControls =
            Item.editing
                { discuss: Discuss
                , input: Input
                , append: Append
                , remove: Remove }
        
        app = 
            App.from site
                |> App.cut route.window -- Route.window is : Lamb
                    |> App.focus route.focus

        item =
            Item.from app

        entry =
            item |> map Item.toEntry 
     
    in
        Interface.compose
            Me.interface
            Route.prologue
            entry
            epilogue                                                        -- -> invisible skeleton
                |> Interface.draw ( Item.drawDataAt site.dataAt )           -- -> static data view
                |> Interface.wire editingControls interactionControls       -- -> interactive interface
                |> Interface.view                                           -- -> Html



    let 
        me : Interface.Meta Locus msg
        = case Me of
             Nobody ->
                { closed: always ( Html.static ( text "me, ta: Nobody." ) ) }
             _      ->
                { closed: always ( Html.static ( text "me, ta: Somebody." ) ) }

-------------------------------------------------------------------------------------------------------------------------------

        withRoute : Interface.Meta -> Interface
        withRoute =
            case route of
             
                Route.Requesting url ->
                    Url.toString url |> text
            
                Route.Failed { invalid, previous } ->
                    
                    div [] 
                        [ Url.toString invalid |> text
                    
                        , case previous of
                           
                            Nothing -> 
                                text " --- Try changing the address."
                    
                            Just url ->
                                span []
                                     [ text "previous was: ", Url.toString invalid |> text ]
                        ]
            
                Route route ->
                    viewSite route
        
        withSite { location, window, focus } =
            case site of
                    
                Site.Loading _ -> 
                    h2 [] [text "site is loading (Route.view)"]
                        
                Site.Failed _ -> 
                    h2 [] [text "site loading failed (Route.view)"]
                        
                Site cache -> 
                    case Site.copy location cache of
                                
                        Nothing ->
                            text "The site you requested was loaded but then lost from your local memory. Refresh your browser."
                                

                        Just { app, basis, sessions } ->
                        
                            Interface.draw
                            
                                { prologue:         -- first entry to items; horizontal movement is possible.
                                , entries: BiZipper ( Entry k a )        -- vertical and horizontal movement.
                                , epilogue: { view: Viewer }
                                , meta: Maybe ( Meta k a )
                                , changer: k -> a -> msg
                                }
                            
                            
                            
-------------------------------------------------------------------------------------------------------------------------------
                                
   in
    { title = "tangible 0.0.9"
    , body = viewSite
    
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























