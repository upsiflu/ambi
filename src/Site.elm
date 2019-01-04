port module Site exposing (Signature)

import App
import Version
import Session
import Edit exposing (zero, edit, Data (..))
import List exposing (map, foldl, concat)
import Dict exposing (get)
import Locus exposing (role)















{----------------------------------------------------------------

    Site, with

  * its signature (immutable)
  * its own App (immutable)
  * a copy of the latest version's cache
  
    referencing

  > its curator
  > its versions, accumulating published sessions
  > its draft, consisting of all unpublished sessions,

    is identified by <curator>/<app>, where curator names an
    Avatar, appSignature an App. This path is its signature.
    
    Each Site keeps a Cache of its single public Version.
    This Cache omits all information that is not nescessary to
    reconstruct the public facade of the site, as intended.
    
    A Site links alternative Versions as well as the current
    draft which consists of all Sessions that target the
    current Version. Route handles these links.
    
    One App is fully included with each site.

 ----------------------------------------------------------------}

 
type Site
    = Loading Signature
    | Failed Signature Problem
    | Site Signature
        { app:App
        , basis:Dict Locus Data 
        , sessions:List Session
        , versions:List Version
        }

type Signature =
        { curator:Avatar.Signature
        , appSignature:String }


value : List Session -> Locus -> Dict Locus Data -> Data
value basis sessions locus =
    sessions
        |> map ( Session.edits locus )
        |> map ( withDefault [] )
        |> concat
        |> foldl edit ( get locus basis )

        
{----------------------------------------------------------------

    A Focus
    
    has a speech bubble underneath for the discussion
    
    has space on top and bottom which can be clicked to unfocus
    
    is a transparent div
    
    has a text field that shares its focus state if Leaf

    if the text field is empty, then media buttons appear
    and also a (-) button on top right if it was added
    
    has a bottom sheet if multiple templates apply.
    Choices are saved to the session.

    is a htable with sticky header if multiple concept definitions
    apply anywhere up the hierarchy.
    The Url syncs the scrollstate of the table
    
    
    

    A Focus
    
    is:
    
  + an Alternation of several Alternatives
    - with an insufficient chain of Choices | render with chooser
  | not an Alternation, or Alternation with final choice
    + a Leaf                                
      + 
    | an actionable Concept                 | render with action

    
    has:
    
  - Data (including Zero)
  - 
  
    may have:
    
  - neighbor Variants on both sides
  

 ----------------------------------------------------------------}



viewItem : App -> Dict Locus Data Locus -> List Session -> Locus -> Html msg
viewItem app basis sessions locus =
    case ( template app locus, value basis sessions locus) of
        ( Naming concept, Zero )
           -> 
        Appended Signature Data     
        Input String Data           
        Choice Type.Alternative Data 
             
