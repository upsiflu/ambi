module Moderation exposing (suggest, assert, withdraw, delete)




















{----------------------------------------------------------------

    Edits in a draft are moderated before publication.
    When adding an edit, you can either suggest or propose it.
    You can also accept suggestions (incl. yours).
    
    This module prevents illegal type constructions.

    ----------------------------------------------------------

    The Moderation Circle

                  suggest a modification   -> Suggested
    Suggested  -> withdraw your suggestion -> Withdrawn
               -> accept any suggestion    -> Canonized

                  propose a modification   -> Canonized
    Canonized  -> delete any canonized m   -> Vestige
    
               -- Only yours: --
    Suggested  -> undo suggest             -> Withdrawn
    Withdrawn  -> undo withdraw            -> Suggested 
    Canonized  -> undo propose             -> Vestige
    
 ----------------------------------------------------------------}

 
type  Moderation m
    = Suggested m
    | Canonized m
    | Published m
    | Withdrawn m
    | Vestige

type  Suggestion m = Suggestion m

    
-- To propose a modification, you have to moderate it first.

suggest modification = Suggested modification
propose modification = Published modification

accept : (Suggested m) -> ()
accept suggestion = Canonized suggestion
withdraw (Moderated modification)



            
