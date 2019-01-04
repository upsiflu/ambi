module Me exposing (Me, nobody) 

import Avatar exposing Avatar




















{----------------------------------------------------------------

    Me

    keep connection betweem 

 ----------------------------------------------------------------}



type Me             = Nobody
                    | Creating Auth Avatar User
                    | Switching Auth Avatar User
                    | Embodying Auth Avatar User
                    



disembody _ = Nobody
nobody      = Nobody


-- embody ...
