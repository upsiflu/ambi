port module Version exposing () 




















{----------------------------------------------------------------
    
    Version, with
    
  * A list of Sessions relative to the previous session
  * A cache with the data to just construct its result,
    
    referencing

  > its previous version,
    
    corresponds to a file on the database.
    
    It stores all sessions relative to the previous version
    and a minimized list of edits that generates its result.
    
    Site duplicates the most recent version's cache.

 ----------------------------------------------------------------}

 
 
 
type Version        = Version Signature
                        { sessions:List Session
                        , cache:Cache
                        , previous:Version.Signature
                        }

type alias Cache = List (Locus, Data)
