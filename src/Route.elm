module Route exposing (Route, visit, view)

import Type exposing (Type, Locus, Word, Perspective, neutral)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, <?>, stringParam)
import Url exposing (Url)
import Site exposing (viewSite, viewFocus)
import Thread exposing (Thread)














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
    
 ----------------------------------------------------------------}
    

type Route          = Requesting Url
                    | Failed { invalid:Url, previous:Maybe Url }
                    | Route
                        { site:Site
                        , window:Locus                      -- /
                        , watch:Maybe String                -- ?
                        , perspectives:List Perspective     -- ;
                        , focus:Locus                       -- #
                        }


--- VIEWING ---

view route =
    case route of
        Requesting url 
            -> Url.toString url |> text
        Failed request 
            -> div [] [ Url.toString request.invalid |> text
                      , case request.previous of
                                Nothing -> text " --- Try changing the address." ]
                                Just url -> span [] [text "previous was: ", Url.toString request.invalid |> text]
                      ]
        Route { routeData | site, window, focus }
            -> case site of
                    Loading _
                        -> h2 [] [text "site is loading (viewItem)"]
                    Failed _
                        -> h2 [] [text "site loading failed (viewItem)"]
                    Site sig { s | app, basis, sessions }
                        -> let
                            withData viewer
                                = viewer app basis sessions
                            before
                                = Locus.before focus window
                                    |> map ( viewItem |> withData )
                            focused
                                = focus |> ( viewFocus |> withData )
                            after 
                                = Locus.after focus window
                                    |> map ( viewItem |> withData )
                           in
                            before ++ focused ++ after |> div [class "thread"]
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
--- NAVIGATING ---




routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [map .... ( WINDOW <?> stringParam "review")]


visit url =
    -- if the app is new, we have to parse a new type and wil return a Requesting.
  let 
      app = map App <| s "tangiblemodel.com" </> string </> t </>
  in
      parsePath site window mode focus
    
    
    
    --split in parts....
    Route {site:load url, window:Locus.neutral, mode:Default, focus:Locus.neutral}
