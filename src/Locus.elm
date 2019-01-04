module Locus exposing (Locus, reverse, neutral, role, lociBefore, lociAfter)

import App exposing (encodeConcept, encodeWord)




















{----------------------------------------------------------------

    Locus
    
    locates any actionable item within a Site.
    It omits the Relations, but may include Edit numbers
    to visit dynamically created items as well as extra steps
    to locate (but keep ambiguous) perspectives.
    
(L) Leaves contain editable data.
(I) Instances are created from Templates. Each is named after
    the signature of the Edit that created it.
(P) If multiple Alternatives define one concept, choose one.
    
    It goes from outside to inside, but you can reverse a locus.
    A locus can describe a rel. or abs. path through your app.
    
    Each data and each edit has exactly one abs. locus.
    Concepts that are actionable (e.g. with +) are also listed.
    Passive concepts are concatenated with their successor.
    
    Example:
    
    a                      -- passive concept; not listed.
     b      -- a/b         -- editable item
     c                     -- passive concept; not listed.
      d     -- a/c/d       -- actionable: +
       +                   -- relation; not listed.
        e   -- a/c/d/0.0.0 -- editable item.
       +
        f
      d
       g

 ----------------------------------------------------------------}



type Locus = Locus (List Step)
type Step  = L Leaf | E Edit.Signature | P Perspective


lociBefore : Locus -> Locus -> List Locus
lociBefore pivot window =

lociAfter : Locus -> Locus -> List Locus
lociAfter pivot window =

type Role =
    

role : App -> Locus -> Role
role app locus=



{----------------------------------------------------------------

    Ambiguity Reduction
    
    
 -- Variants ----------------------------------------------------
    
    Add a perspective to your locus in order to glance at one of
    the definition of an ambiguous concept.
    
    A Concept ambiguity multiplies aspects over Variants.
    Use it to build a table. While rows are focused like
    any item, a Perspective chooses the 'column' in a table.
 
 
 -- Alternatives ------------------------------------------------
    
    If the Template of an App is ambiguous, we get an
    Alternation. Switch between Alternatives to Choose one
    and store it in the Edit.

 
 ----------------------------------------------------------------
     
    Ambilang knows four and half types of ambiguity:
    
    (1) Conflicting Concept Definitions
            Encoding: App.Variation; App.Variant
           Reduction: via Locus.Perspective
       Reinstatement: via Locus.Perspective=All
                  UI: a scrollable table
    
--  (1.5) TODO: Sequence
            Encoding: App.Succession; App.Current
                  UI: a transformation button? Or a horiz. Shift?
    
    (2) Multiple Template definitions for a user-added instance
            Encoding: App.Alternation; App.Alternative
           Reduction: via Edit.Choice
       Reinstatement: via undo (only the curator can reambiguate)
                  UI: a dismissible dialog
                  
--  (3) TODO: Accidental Data Ambivalence
              Source: Concurrent edits on the same context
            Encoding: Edit.Ambivalence; Edit.Sample
           Reduction: via Edit.Fix
                  UI: a dialog in Review mode
       Reinstatement: via Edit.Flux

--  (4) TODO: Explicit Data Ambivalence
              Source: either accidental data ambivalence
                      or any edit if this mode is on
            Encoding: Edit.Embrace Edit.Ambivalence
                      Locus.editing=createAmbivalence
           Reduction: via Locus.AmbiFilter
                  UI: perhaps a filter toolbox?
                      Or an unordered list?
                      Or an expressionist graphic?
       Reinstatement: via Edit.Reflux (deembraces flux)
        
 ----------------------------------------------------------------}


type Perspective = Perspective { options:Variation, choice:Variant }


    
    
    
    
-- ENCODING --



type alias Path = List String

encode : Locus -> Path
encode ( Locus step:s ) =
    encode s 
    |> (::) case step of
         L l -> encodeLeaf l
         E e -> Edit.signature e
         P p -> encodePerspective p
         

encodePerspective (Perspective variation variant) =
    (encodeLocus variation) ++ "=" ++ (encodeDefinition variant) 

encodeDefinition (Definition group) =
    group |> map encodeWord |> String.join (",")

