module Avatar exposing (Avatar, Signature)






















{----------------------------------------------------------------

    Avatar


 ----------------------------------------------------------------}


type Avatar        
    = Loading Signature
    | Failed Signature Problem
    | Novatar Signature
    | Avatar Signature AvatarPicture
    
type Signature      = Signature String
type AvatarPicture  = AvatarPicture String
