import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Dict exposing (Dict)

import UI exposing ( .. )
import App exposing ( .. )
     

-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

  
   
-- MODEL

type alias State =
    {  }

type alias Model =
  { key: Nav.Key
  , app: App
  , focus: List String
  , url: Url.Url
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key App.initialApp [] url, Cmd.none )

 

-- UPDATE
 

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Back
    | PutFocus ( List String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )
    
    Back -> ( model, Nav.back model.key 1 )
    PutFocus k -> 
        ( { model | focus = k }
        , Cmd.none
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- UI

initialUI : UI String Model Msg
initialUI =
 UI.create       
    {     
    -- reading a Url
      router = identity -- for strings
    , windowKeys =    always []
    , intendedFocus = \state -> state.focus
    , prologue = always [ text "Prologue" ]
    }
    {
    -- items over keys respond to state.
      drawPassiveItem =    
        (\stack state ->
            [ App.getString stack state.app |> text ] )
    , drawInteractiveItem =
        (\stack state ->
           UI.ring  
            { interactivity =    
                UI.Link { target = "", description = App.getString stack state.app }
            , representation =
                [ text "FOCUS" ] } [] )
    , getChildKeys =
        (\stack state ->
            App.getFirstSteps stack state.app )
    }
    {  
    -- Providing these Messages for internal navigation
      putFocus = (\stack -> PutFocus stack )
    , getLink = (\stack -> "" )
    , back = Back
    --------------------------------------------------
    , epilogue = [ text "epilogue" ]
    , meta = [ text "meta" ]
    --------------------------------------------------
    }
 
 
-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Milestone0.1"
  , body =   
      [ h3 [] [ text (Url.toString model.url) ]
      , initialUI |> UI.view model
      ]
  } 
