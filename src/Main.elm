module Main exposing (..)

import Browser.Navigation as Navigation
import Url exposing (Url)

import Type exposing (Type)
import Me exposing (Me, nobody)
import Route exposing (visit, view)













{----------------------------------------------------------------
    
    Main
    
    generates a runtime.
    It maintains a Route (which animates an URL)
    and can be extended by Me (the choreographer/user) as author.
    
 ----------------------------------------------------------------}
    

type alias Model
   = {route:Route, author:Me, key:Navigation.Key}

init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key = (Model Route.visit(url) Me.nobody key, Cmd.none)

    
    
---- UPDATE ----


type Msg
    = 
    | 
    | UrlRequested Browser.UrlRequest
    | UrlChanged   Url
    
    
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlRequested urlRequest ->
        case urlRequest of
            Browser.Internal url -> (model, Navigation.pushUrl model.key)
            Browser.External href -> (model, Navigation.load href)
    UrlChanged new -> ( { model | route = Route.visit new }, Cmd.none )


    



---- VIEW ----


view : Model -> Html Msg
view model =
  let 
      viewMe
      = case Me of
             Nobody -> text "nobody"
             _      -> text "somebody"
  in
    { title = "tangible 0.0.9"
    , body =
      div []
          [ h1 [] [ text "Milestone 9: Navigation, Edits, and Modules Rewrite" ]
          , 









---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    , onUrlRequest = RequestedNavigation
    , onUrlChange  = UrlChanged
    }























