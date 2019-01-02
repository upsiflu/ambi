port module Avatar exposing (Signature)






















{----------------------------------------------------------------

    Avatar


 ----------------------------------------------------------------}


type Avatar        
    = Loading Signature
    | Novatar Signature
    | Avatar Signature AvatarPicture
    | Failed Signature Problem
    
type Signature      = Signature String
type AvatarPicture  = AvatarPicture String



disembody _ = Nobody
nobody      = Nobody
