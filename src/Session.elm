port module Session exposing
    ( Session, encode, decoder, create, commit
    
      -- readable --
    , signature, contact, serverTime, edits, highestOrdninal
    )
 
import Edit


















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
    | Failed Session Problem
    | Session SessionData

type alias SessionData =
    { signature:Signature
    , contact:String
    , serverTime:(Maybe Time)
    , loci:Dict Locus Contributions
    }

    
type alias Contributions =
    { edits:Edits, discussions:Discussions }
    
type Signature          -- a Nominal only created by the server
    = Signature Int
    | Requested

    
    
type alias Edits = List (Edit)
type alias Discussions = List (Discussion)

decoder : Decoder Signature -- from JSON 
encode : Signature -> Value -- to JSON



create
    = \suppliedContact -> Session { suppliedContact | signature:Requested, serverTime:Nothing, edits:[], mine:True}
    -- and then request a unique nominal via port.

read
    = \suppliedSignature -> Loading suppliedSignature

serverResult : Session -> ServerData -> Session
serverResult session result
    = case result of
           Error e -> Failed session e
           Success s ->
            case session of
                 Loading signature -> Session { signature:signature, contact:decodeContact s, serverTime:decodeTime s, edits:decodeEdits s, mySession:false }
                 Session provided -> Session { provided | serverTime:decodeTime s, edits:decodeEdits s }


signature: Session -> Signature
contact: Session -> User.Contact
serverTime : Session -> Time

-- per locus --
edits : Locus -> Session -> Maybe Edits
edits locus session =
    case session of
         Loading _ -> Nothing
         Failed _  -> Nothing
         Session s -> s.loci |> get locus |> Maybe.map .edits
             

discussions : Locus -> Session -> Maybe Discussions
highestOrdinal : Locus -> Session -> Edit.Ordinal


commit: Edit -> Locus -> Session { SessionData | mine } -> Result
commit edit locus session =
    case session of
         Loading _ -> Error "session is not loaded yet"
         Failed _  -> Error "session failed loading"
         Session s ->
            -- ask firebase to insert 
