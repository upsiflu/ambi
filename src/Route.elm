module Route exposing (Route, visit, view)


import App exposing (Type, Locus, Word, Perspective, neutral)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, <?>, stringParam, top)
import Url exposing (Url)
import Site exposing ( showSignature )

import Html exposing (h1, text)













{----------------------------------------------------------------

    Route, referencing
    
  > a Site
  > a window into the app
  > flags to persist auxiliary navigation
  > a focus within the window,
  
    corresponds exactly to a valid URL.
    
    Use this module
    to safely navigate within a given site.
    Route draws its site with its flags.

    Example: tangiblemodel.com/
             flupsi/portfolio/
             ?watch=me;language=en
             #post.0/1/3

    There are no route errors, meaning that every exposed
    function in this module produces unwrapped values.
    
    Therefore, they can only yield either Errors from other
    modules or empty wrappers ( String, Zipper... ).

    It also means that Locus must not encode more than its serialized form.
    
 ----------------------------------------------------------------}
    

type alias Route =
    { domain: { protocol: Protocol, host: String, port_: Maybe Int  }
    , location: Site.Signature
    , window: Locus                      -- /
    , focus: Locus                       -- #
    , optionals: Optionals               -- this is not always given.
    }

type Optionals
    = Requesting Url
    | Failed { invalid:Url, previous:Maybe Url }
    | Parsed { perspectives: List Perspective }
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
--- NAVIGATING ---


-- always returns a loading route and the appropriate requests for data.

new : Url -> ( Value -> Cmd msg ) -> ( Route, Cmd msg )
new url outgoing =
    let
        locationParser : Parser ( Site.Token -> a ) a
        locationParser =
            string </> string |> map Site.Token
        
        windowParser : Parser ( Locus -> a ) a
        windowParser =
            string </> string |>
        
        segments =
            { domain: { protocol: url.protocol, host: url.host, port_: url.port_ }
            , location: parse locationParser url.path |> Site.load
            , window: parse windowParser url.path
            , watch:
            , perspectives:
            , focus: parse windowParser url.fragment
            }
        question = encodeQuestion...
    in
        ( Requesting url, outgoing question )

-- 

getDomain url = { protocol: url.protocol, host: url.host, port_: url.port_ }

visit : Url -> ( Value -> Cmd msg ) -> Maybe Route -> ( Route, Cmd msg )
visit url outgoing oldRoute =
    case oldRoute of
        Nothing ->
            new url outgoing
        
        Just Requesting Url ->
            new url outgoing
        
        Just Failed { invalid, previous } ->
            new url outgoing
        
        Just Route
            { domain
            , location
            , window
            , watch
            , perspectives
            , focus
            } as old ->
            case old of
                 { o | domain: getDomain url }
                 
            
            case domain of
                 ->
                    
                newDomain ->
                    new url outgoing
            




-- if the app is new, we have to parse a new type and wil return a Requesting.
  let 
      app = map App <| s "tangiblemodel.com" </> string </> t </>
  in
      parsePath site window mode focus
    
    
    
    --split in parts....
    Route {site:load url, window:Locus.neutral, mode:Default, focus:Locus.neutral}

    
    
    
    
routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [map .... ( WINDOW <?> stringParam "review")]


----------------------------------------------------------------

-- VIEW

viewPrologue : Route -> Html Never
viewPrologue route = route.location |> text Site.showSignature |> h1 []
