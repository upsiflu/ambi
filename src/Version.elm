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




type Id = Id -- Tag used for all Versions

type alias Signature = -- Tag specific to one Version
    Tagged Id Token

type alias Token =
    Int

type alias Cache =
    TDict Signature Copy
   
   
   
   
type alias Copy =
    { sessions: Session.Cache
    , state: Dict Locus Edits
    , previous: Version.Cache
    }

    
    
    
    







--- Data from the Server arrived ---------------------------------


sign : Token -> Signature
sign token = Tagged Id token

populate : Value -> Token -> Version
populate json signature =
    decode json |> singleton ( sign token ) |> Version

fail : Problem -> Token -> Version
fail problem token = sign token -> Failed problem

    


--- Managing several copies ---------------------------------------


combine : Cache -> Cache -> Cache
combine a b = b
