module Locus exposing (Locus, reverse, neutral)

import Type.encodeConcept




















{----------------------------------------------------------------

    Locus
    
    locates any actionable item within a site.
    It omits the Relations, but may include Edit numbers
    to visit dynamically created items.
    
    It goes from outside to inside, but you can reverse a locus.
    A locus can describe a rel. or abs. path through your type.
    
    In case there is an ambiguous concept, it may disambiguate
    this step (see Perspective).
    
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

 ----------------------------------------------------------------}

 
type Locus = Locus (List Step)
type Step  = C Concept | E Edit | P Perspective


encode ( Locus steps )
   = steps
   |> map \step ->
         case step of
            C c -> encodeConcept c
            E e -> Edit.signature e
            P p -> encodePerspective p
         
{----------------------------------------------------------------

    Ambiguity Reduction
    
    
 -- Alternatives ------------------------------------------------
    
    If the template of a type is ambiguous, we get an
    Alternation. Switch between Alternatives to Choose one
    and store it in the Edit.

    
 -- Variants ----------------------------------------------------
    
    Add a perspective to your locus in order to glance at one of
    the definition of an ambiguous concept.
    
    A concept ambiguity multiplies aspects over 'variants'.
    Use it to build a table. While rows are focused like
    any item, a Perspective chooses the 'column' in a table.
     
     
 ----------------------------------------------------------------
     
    Ambilang knows four and half types of ambiguity:
    
    (1) Conflicting Concept Definitions
            Encoding: Type.Variation; Type.Variant
           Reduction: via Locus.Perspective
       Reinstatement: via Locus.Perspective=All
                  UI: a scrollable table
    
--  (1.5) TODO: Sequence
            Encoding: Type.Succession; Type.Current
           Reduction: via Locus.Perspective
       Reinstatement: via Locus.Perspective=All
                  UI: a transformation button? Or a horiz. Shift?
    
    (2) Multiple Locus Templates
            Encoding: Type.Alternation; Type.Alternative
           Reduction: via Edit.Choice
       Reinstatement: via undo (only the author can reambiguate)
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


type Perspective = Perspective Variation Variant
    

encodePerspective (Perspective variation variant) =
    (encodeLocus l) ++ "=" ++ (encodeDefinition variant) 

encodeDefinition (Definition group) =
    "{" ++ (List.join "," group) ++ "}"

    
