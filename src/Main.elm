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
    { route:Route
    , curator:Me
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

    
    
---- UPDATE ----


type Msg
    = Incoming Value 
    | Outgoing Value
    | UrlRequested Browser.UrlRequest
    | UrlChanged   Url
    
    
    

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
view { route, site } =
    let 
        viewMe
        = case Me of
             Nobody -> text "nobody"
             _      -> text "somebody"

-------------------------------------------------------------------------------------------------------------------------------

        viewRoute =
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
        
        viewSite { location, window, focus } =
            case site of
                    
                Site.Loading _ -> 
                    h2 [] [text "site is loading (Route.view)"]
                        
                Site.Failed _ -> 
                    h2 [] [text "site loading failed (Route.view)"]
                        
                Site cache -> 
                    case Site.retrieve location cache of
                                
                        Nothing ->
                            text "The site you requested was loaded but then lost from your local memory. Refresh your browser."
                                
                        Just { app, basis, sessions } ->

                            let
                                withData viewer
                                    = viewer app basis sessions
                               
                                before
                                    = Locus.before focus window
                                            |> map ( viewItem |> withData )
                                focused
                                    = focus 
                                        |>    ( viewFocus |> withData )
                                after 
                                    = Locus.after focus window
                                        |> map ( viewItem |> withData )

                            in
                                before ++ focused ++ after |> div [class "thread"]

                                
                                
                                
{----------------------------------------------------------------

        Drawing an Item
        
    There is one rule that can guide us through this process:
    "each piece of state that is there should be communicated"
    
    - the law of information preservation -.
    
    So why not start with a list or tree or record
    of all available information
    and **map** this to representation?
    
    At least conceptually we should do this.


    A Focus
    
    has a speech bubble underneath for the discussion
    
    has space on top and bottom which can be clicked to unfocus
    
    is a transparent div
    
    has a text field that shares its focus state if Leaf

    if the text field is empty, then media buttons appear
    and also a (-) button on top right if it was added
    
    has a bottom sheet if multiple templates apply.
    Choices are saved to the session.

    is a htable with sticky header if multiple concept definitions
    apply anywhere up the hierarchy.
    The Url syncs the scrollstate of the table
    
    
    

    A Focus
    
    is:
    
  + an Alternation of several Alternatives
    - with an insufficient chain of Choices | render with chooser
  | not an Alternation, or Alternation with final choice
    + a Leaf                                
      + 
    | an actionable Concept                 | render with action

    
    has:
    
  - Data (including Zero)
  - 
  
    may have:
    
  - neighbor Variants on both sides
  

 ----------------------------------------------------------------}



        viewItem : ist msg -> App -> Dict Locus Data Locus -> List Session -> Locus -> Html msg
        viewItem actions app basis sessions locus =
            case ( template app locus, value basis sessions locus) of
                ( Naming concept, Zero ) ->
            
                Appended Signature Data     
                Input String Data           
                Choice Type.Alternative Data 

-------------------------------------------------------------------------------------------------------------------------------
                                
   in
    { title = "tangible 0.0.9"
    , body =
     div []
       [ h1 [] [ text "Milestone 9: Navigation, Edits, and Modules Rewrite" ]
       , viewRoute
       ]
    
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























