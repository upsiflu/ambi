port module Site exposing (Signature)

import Type



















{----------------------------------------------------------------

    Site

    A site represents the corresponing database item.
    It is identified by <curator>/<app>, where curator is an
    avatar and app is a type.
    
    Each site keeps a cache of its 'current' version (0).
    This cache omits all information that is not nescessary to
    reconstruct the public facade of the site, as intended.
    
    A site links alternative versions as well as the current
    draft which consists of all sessions that target the
    current version. Route handles these links.
    
    Type is fully included with each site.

 ----------------------------------------------------------------}

 
type Site
    = Loading Signature
    | Failed Signature Problem
    | Site Signature {curator:Avatar.Signature, app:Type, 

create
