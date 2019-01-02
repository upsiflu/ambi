port module Session exposing
    ( Session, encode, decoder, create, commit
    
      -- readable --
    , signature, contact, serverTime, edits, highestOrdninal )
 
import Edit


















{----------------------------------------------------------------

    Session

    is part of a drafting process. A session begins when both
    a user, a site and an embodiment are given.
    
    It stores a contact and a unique Signature (a Nominal),
    which is created by the server. A list of Edits, organized
    by Locus, stores all previous Edits.
    
    *read* a Session to trace what other users are writing.
    *create* a Session to make it your own.
    *commit* an edit to prepend it to your session.

 ----------------------------------------------------------------}


type Session
    = Loading Signature
    | Failed Session Problem
    | Session SessionData

type alias SessionData = {signature:Signature, contact:User.Contact, serverTime:(Maybe Time), edits:Edits}

type Signature          -- a Nominal only created by the server
    = Signature Int
    | Requested

    
    
type alias Edits = List (Locus, List (Edit))

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
edits : Session -> Edits
highestOrdinal : Session -> Edit.Ordinal


commit: Edit -> Session { SessionData | mine } -> Session
commit edit (Session previousData)
    = Session { previousData | edits=edit::previousData.edits }













