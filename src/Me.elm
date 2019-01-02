module Me exposing (Me, nobody) 






type Me             = Nobody
                    | Creating Auth Avatar
                    | Switching Auth Avatar
                    | Embodying Auth Avatar
                    
type Avatar         = Novatar
                    | Avatar AvatarID AvatarPicture
                    
type alias AvatarID = String

type AvatarPicture  = AvatarPicture String




disembody _ = Nobody
nobody      = Nobody


-- embody ...
