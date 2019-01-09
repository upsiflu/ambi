module Version exposing
    (
    
    -- read only --
      Site, Signature
    , Item, item
    
    -- write from JSON --
    , populate, fail
    
    -- map --
    , union
    
    )

import Tagged.Dict exposing (singleton, union)
import Tagged exposing (Tagged)











{----------------------------------------------------------------
    
    Version, with
    
  * A list of Sessions relative to the previous session
  * A state with the data to just construct its result,
    
    referencing

  > its previous version,
    
    corresponds to a file on the database.
    
    It stores all sessions relative to the previous version
    and a minimized list of edits that generates its result.
    
    Site duplicates the most recent version's cache.

 ----------------------------------------------------------------}

 
type Version
    = Loading Signature
    | Failed Problem Signature
    | Version Cache




type Signed = Signed -- Tag used for all Versions

type alias Signature = -- Tag specific to one Version
    Tagged Signed Token

type alias Token =
    Int

type alias Cache =
    Tagged.Dict Signed Token Copy
   
   
   
   
type alias Copy =
    { sessions: Session.Cache
    , state: Dict Locus Edits -- This is a shadow of all sessions, minus discussions and contacts. Intended for public.
    , previous: Version.Signature
    }



    
    







--- Data from the Server arrived ---------------------------------


sign : Token -> Signature
sign token = Tagged Signed token

populate : Value -> Signature -> Version
populate json signature =
    decode json |> singleton signature |> Version

loading token = sign token |> Loading

fail : Problem -> Token -> Version
fail problem token = sign token -> Failed problem

    


--- Managing several copies ---------------------------------------


combine : Cache -> Cache -> Cache
combine a b = b
