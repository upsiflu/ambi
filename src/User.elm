port module User exposing (contact)

import Url




















{----------------------------------------------------------------
    
    User (stateful), with
    
  * default contact (immutable)
  * whether she is discussing or editing
  * URL history
    
 ----------------------------------------------------------------}

 
type User =
    User Signature { contact:String, activity:Activity, history:List Url.Url }

type Activity = Discussing | Editing
