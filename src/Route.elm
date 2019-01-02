module Route exposing (Route, visit, view)

import Type exposing (Type, Locus, Word, Perspective, neutral)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, <?>, stringParam)
import Url exposing (Url)
















{----------------------------------------------------------------

    Route
    corresponds exactly to a valid URL.
    
    use this module
    to safely navigate within a given site.
    Route draws its site with its flags.

    Example: tangiblemodel.com/
             flupsi/portfolio/
             ?review=0&language={en}
             #post.0/1/3
    
 ----------------------------------------------------------------}
    

type Route          = Route {site:Site, window:Locus, flags:RouteFlags, focus:Locus} | Requesting Url
type RouteFlags     = RouteFlags {review:ReviewMode, perspectives:List Perspective}
type ReviewMode     = Reviewing | Default


-- A route hosts one 
type Site           = Site  {app:App, version:Version, draft:Draft}

type Version        = Version {id:Int, edits:List Edit}
type Draft          = Draft {my:Session, other:List Session}
type alias Session  = {id:Int, contact:String, edits:List Edit}
type App            = App {avatarID:String, model:Type}






--- VIEW ---

view : Route -> Html msg
view route
 = 

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
