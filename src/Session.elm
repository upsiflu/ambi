module Session exposing
    (
    
    -- read only --
      Site, Signature
    , Item, item
    
    -- write from JSON --
    , populate, fail
    
    -- map --
    , union
    
    )

import Edit
import Tagged.Dict exposing (singleton, union)
import Tagged exposing (Tagged)










{----------------------------------------------------------------

    Session (stateful), with
    
  * Nominal, a unique increment doubling as name (immutable)
  * contact info
  * the server time at creation (immutable)
  * Edits during the session (append-only)
  * Discussions during the session (append-only),

    begins when a user, a site and an embodiment are given.
    
    It stores a contact and a unique Signature (a Nominal),
    which is created by the server. Lists of Edits and
    Discussions, each organized by Locus, store its history.
    
    *read* a Session to trace what other users are writing.
    *create* a Session to make it your own.
    *commit* an Edit.
    *contribute* a Discussion.
    
    TODO: Constraints create helper apps that send out data
    into the main app in order to relate and limit values.

 ----------------------------------------------------------------}


type Session
    = Loading Signature
    | Failed Problem Signature
    | Session Cache




type Id = Id -- Tag used for all Sessions

type alias Signature = -- Tag specific to one Session   
    Tagged Id Token

type alias Token =
    Int

type alias Cache =
    Tagged.Dict Id Token Copy

    


type alias Copy =
    { contact: String
    , serverTime: (Maybe Time)
    , loci: Dict Locus Contributions
    }
    
type alias Contributions =
    { edits:Edits
    , discussions:Discussions }
type alias Edits = List Edit
type alias Discussions = List Discussion







--- Data from the Server arrived ---------------------------------


sign : Token -> Signature
sign token = Tagged Id token

populate : Value -> Token -> Session
populate json signature =
    decode json |> singleton ( sign token ) |> Session

fail : Problem -> Token -> Site
fail problem token = sign token -> Failed problem

    


--- Managing several copies ---------------------------------------


combine : Cache -> Cache -> Cache
combine = union






decoder : Decoder Signature -- from JSON 
encode : Signature -> Value -- to JSON






serverResult : Session -> ServerData -> Session
serverResult session result
    = case result of
           Error e -> Failed session e
           Success s ->
            case session of
                 Loading signature -> Session { signature:signature, contact:decodeContact s, serverTime:decodeTime s, edits:decodeEdits s, mySession:false }
                 Session provided -> Session { provided | serverTime:decodeTime s, edits:decodeEdits s }


                 
                 
contact: Session -> User.Contact
serverTime : Session -> Time



-- per locus --
edits : Locus -> Copy -> Maybe Edits
edits locus copy =
    copy.loci |> get locus |> Maybe.map .edits
             

discussions : Locus -> Session -> Maybe Discussions
highestOrdinal : Locus -> Session -> Edit.Ordinal


commit: Edit -> Locus -> Session { SessionData | mine } -> Result
commit edit locus session =
    case session of
         Loading _ -> Error "session is not loaded yet"
         Failed _  -> Error "session failed loading"
         Session s ->
            -- ask firebase to insert 
