module Edit exposing
    (Edit, generator, Data (..)
    
    -- readable --
    , signature )

import Moderation
import Locus














{----------------------------------------------------------------
    
    Edit
    
    An Edit is a Moderated Modification to a specific Locus.
    In order to propagate the user's intent, the context of an
    edit, together with an increasing Ordinal, form a Signature.
    
    Edits are shared over the network and may arrive in wrong
    sequence. The Context reinstates the intended sequence.
    
    If two sessions edit the same locus concurrently, ambiguities
    may arise. The Review panel can be used to either resolve
    ambiguities or lift them to data-level Ambivalence.
    For completeness, you can also construct ambiguity locally.
    
 -- Composability of Edits --------------------------------------
 
    A modification is anything a user can do with data.
    It depends on the data which modifications are available.
    An Edit is a Modification integrated into a chain.
    Data is the result of a chain of Edits.
 
    For a given locus, a Data object of Zero is constructed.
    Its type signature limits which modifications can be applied.
    It is the locus of data specifies its type.
    
    When parsing edits, the Zero Data is encapsulated in more
    data objects, registering only positive changes.
    
 ----------------------------------------------------------------}


type Edit
    = Edit Signature Modification


type Modification ------------ TODO: LOCUS ------ PARAMETER limits ----------
    = Append                      -- appendable   
    | Undo Edit                   -- --           mine; to this
    | Remove Edit                 -- --           appended to this
    | Put String                  -- leafy        --
    | Choose PrototypeChoice      -- Alternation  is choice within locus 


type Data -------------------- TODO: LOCUS ------ PARAMETER limits ----------
    = Zero                        -- --
    | Appended Signature Data     -- appendable   
    | Input String Data           -- leafy
    | Choice PrototypeChoice Data -- Alternation  is choice within alternative groups (Prototypes) 
        

type Signature = Signature {o:Ordinal, n:Session.Name, context:Session.Name}
 
type Ordinal = Ordinal Int -- the depth of the tree for a specific Edit.






{----------------------------------------------------------------
    
    Reducing Edits to Data
    
    Since conceptually, edits are added on top of each other,
    and they come in a definite order, we only need to define
    how any one edit is applied on one data.
    
 ----------------------------------------------------------------}

zero = Zero
 
edit : Edit -> Data -> Data
edit (Edit sig modification) dat =
    let
        without trash from =
            case from of
                 Zero -> from
                 trash inner -> inner
                 good  inner -> without trash inner |> good
    in
        dat |>
            case modification of
                Append
                    -> Appended sig
                Remove (Edit sig _)
                    -> without (Appended sig)
                Put string
                    -> Input string
                Choose alternative
                    -> Choice alternative
                Undo (Edit sig toTrash)
                    -> case toTrash of
                        Append
                            -> without (Appended sig)
                        Remove (Edit restoreSig _)
                            -> Appended restoreSig
                        Put string
                            -> without (Input string)
                        Choose alternative
                            -> without (Choice alternative)
                        Undo _ -> always -- actually, should never happen.


type Template
    = Ambiguous Alternation
    | Exactly Parameters

template : Locus -> Template
template instance
        

        
        
        
                {- different version, using folds and filters 

reduce : (List Edit) -> Data
reduce edits =
    let
      
      -- fold undos, removes; make fit for viewing.
      findRevertion e =
          case e.modification of
                Remove r -> Just r.signature
                Undo r   -> Just r.signature
                _        -> Nothing
      isReverted e = edits |> filterMap findRevertion |> List.member e.signature
      
      -- only go back to the most recent basis.
      trim e:ee = 
          case e.modification of
                Put s -> [e]
                m     -> m:(trim ee)
      
      wrapData d e
          case e.modification of
                Append -> Appended e.signature d
                Put s -> Input s d
                Choose a -> Choice
    in
      edits
        |> filter ( not << isReverted )
        |> trim
        |> foldl wrapData Zero
        
        
                -}

 
 
{----------------------------------------------------------------

    Data
    
    As edits are reduced, data is being generated/edited.
    For efficiency, we reduce from now into the past.
    
    Data currently includes primitives for representing

    - Input
        for type of Leaf.
    - Choice
        for Type of Alternation;
        can be any of its Alternatives.
    
    TODO: Later, we will use tags to limit functions.
    
 -- Encoding Data Ambiguities -----------------------------------
    
    Of the five ambiguities that an app can represent
    (see Locus.elm), two arise from an ambivalent result in
    reduce:
    
    - Concurrent, conflicting edits result in Ambivalence.
      You can manually reduce them to one Sample, or 'embrace'
      the Ambivalence to 'convene' the set of conflicting edits.
    
    - For completeness, you can also construct embraced
      ambivalences in your local app by pressing 'convene'
      on individual edits (available in Review mode).
      Manually undo or 'disperse' these edits to make them
      vanish in history again. Both disperse and convene work
      on sets as well.

 ----------------------------------------------------------------}

 
  

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
-- Modification types --
type Anchor                 = Anchor String   | BlankAnchor
type Input                  = Input  String   | BlankInput
type Media                  = Media  String   | BlankMedia
type Addition               = Increment       | Zero

increment : (Zipper Addition) -> (Zipper Addition)
increment additions = insert (Tree.singleton Increment) additions |>
  open ((==) Increment) |> Maybe.withDefault (Increment |> Tree.singleton |> Zipper.fromTree)

reduceA a = case (current a) of
  Anchor    s -> Anchor s
  BlankAnchor -> BlankAnchor

reduceI i = case (current i) of
  Input     s -> Input s
  BlankInput  -> BlankInput

reduceM m = case (current m) of
  Media     s -> Media s
  BlankMedia  -> BlankMedia

reduceC c = case (current c) of
  Just      t -> Just t
  Nothing     -> Nothing

-- Each Edit is saved in the DB.
Edit modification
   = Reset
   | Propose  (Moderate modification)
   | Undo     (Edit modification)
   | Published modification

