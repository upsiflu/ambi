module Locus exposing (Locus, reverse, neutral, role, lociBefore, lociAfter)

import App exposing (encodeConcept, encodeWord)




















{----------------------------------------------------------------

    Locus
    
    is an address in an app. Every locus can have data and/or
    discussions attached. Objects added with a + receive the
    name of the edit that created them.
    
    A Concepts whose definition is only one concept are 'transitive'
    in that they stick with their child.
    
    Example:
    
    a                      -- Rubric (has multiple children).
     b      -- a-b         -- editable.
     c                     -- transitive concept; not listed.
      d     -- a-c/d       -- actionable: +; Multidefinition (Arrangement).
       +                   -- Relations are not listed.
        e   -- a-c/d/0.0.0 -- editable; removable.
       +
        f                  -- swallowed by g.
         g  -- a-c/d/1.0.0 -- editable.
      d                    -- same as the other d.
       g    -- a-c/d/g     -- editable.

 ----------------------------------------------------------------}



type alias Locus = List Step
type alias Step  = List String



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

    
-- ENCODING --



type alias Path = List String
