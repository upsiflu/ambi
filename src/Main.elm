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
    { curator: Me
    , route: Route
    , site: Site
    , local: Local
    , key: Navigation.Key
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

--- INTERFACE --------------------------------------------------------
--- ERRORS ----

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
    | Discuss Locus
    | Decide Locus

    --- UI
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
view { me, route, site, local } =
    
    let
        navigator =
            Interface.Navigator
                { dismiss: Dismiss
                , focus: Focus
                , blur: Blur
                , meta: Menu
                , context: Context }

        editor =
            Kind.editor
                { discuss: Discuss
                , input: Input
                , append: Append
                , remove: Remove }
        
        items : Zipper Interface.Item
        items =
            route
            |> Route.siteToken >> Site.copy
            |> Result.map .app                                 

            |> Result.Extra.extract App.siteErrorReporter -- An App that displays the site errors. Site error must include the erred site.
            |> App.skip route.window 
            |> App.walk route.focus

            |> Zipper.map ( Kind.fromApp >> Kind.toItem )
            

        withSyncIndications : Interface.Drawer -> Interface.Drawer
        withSyncIndications =
            local
            |> Local.syncStateIndicator
            |> Interface.asAnnotator


    in
        Interface.Skeleton
            Me.draw
            Route.prologue
            items
            epilogue                                                   -- -> invisible skeleton
            
            |> Interface.draw ( Kind.drawer |> withSyncIndications )   -- -> static data view
            |> Interface.wire navigator editor                         -- -> interactive interface
            |> Interface.view                                          -- -> Html



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























