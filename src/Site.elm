module Site exposing 
    (
    
    -- read only --
      Site, Signature
    , Item, item
    
    -- write from JSON --
    , populate, fail, loading
    
    -- map --
    , union
    
    )

import App
import Version
import Session
import Edit exposing (zero, edit, Data (..))
import List exposing (map, foldl, concat)
import Dict exposing (get)
import Locus exposing (role)
import Tagged.Dict exposing (singleton, values, get)
import Tagged exposing (Tagged)




{----------------------------------------------------------------

    Site, with

  * its signature (immutable)
  * its own App (immutable)
  * a copy of the latest version's cache
  
    referencing

  > its curator
  > its versions, accumulating published sessions
  > its draft, consisting of all unpublished sessions,

    is identified by <curator>/<app>, where curator names an
    Avatar, appSignature an App. This path is its signature.
    
    Each Site keeps a Cache of its single public Version.
    This Cache omits all information that is not nescessary to
    reconstruct the public facade of the site, as intended.
    
    A Site links alternative Versions as well as the current
    draft which consists of all Sessions that target the
    current Version. Route handles these links.
    
    One App is fully included with each site.

 ----------------------------------------------------------------}


type Site
    = Loading Signature
    | Failed Problem Signature
    | Site Cache




type Id = Id -- Tag used for all Sites.

type alias Signature = -- Tag specific to one Site.
    Tagged Id Token

type alias Token =
    ( Avatar.Signature, App.Signature )

type alias Cache =
    Tagged.Dict Id Token Copy -- A Dict for Token -> Copy, but with keys that are minted by Site.

    


type alias Copy =
    { app: App
    , basis: Dict Locus Data 
    , sessions: Session.Cache
    , versions: List Version
    }
        





        


        
--- Data from the Server arrived ---------------------------------


sign : Token -> Signature
sign token = Tagged Id token

populate : Value -> Token -> Site
populate json signature =
    decode json |> singleton ( sign token ) |> Site

loading = Loading

fail : Problem -> Token -> Site
fail problem token = sign token -> Failed problem




--- Managing several copies ---------------------------------------


combine : Cache -> Cache -> Cache
combine cacheA cacheB = cacheB



retrieve : Signature -> Cache -> Maybe Copy
retrieve signature = get signature
    
dataAt : Copy -> Locus -> Data
dataAt { copy | basis, sessions } locus =
    sessions
        |> values                        -- [Session.Copy]
        |> map ( Session.edits locus )   --  Maybe Edits
        |> map ( withDefault [] )        --  or at least []
        |> concat                        -- Edits from all session copies
        |> foldl edit ( basis |> get locus )  -- onn top of basis

        
             
