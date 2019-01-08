module App exposing (App, Leaf, Variation, Alternation, Definition, Parameters, encodeConcept)

import List
import Lazy.Tree as Tree exposing (Tree(..), Forest)
import Lazy.Tree.Zipper as Zipper exposing (Zipper, current, fromTree, children, root, openAll)
















{----------------------------------------------------------------
    
    App
    
    is an immutable structure, written in ambilang,
    known to the computer in the form of a lazy rose-tree.
    
    The use of the Zipper datastructure means that costly
    inferences on the app can be deferred and no initialization
    needs to be triggered. Instead, the Zipper maintains a
    construction function 'on demand'.

 -- Constellations of an App ------------------------------------
    
    Indentations denote parent-child relation,
    Symbols +, <, >, : denote mappings over children,
    any other word denotes a concept.
    
    A Group may be tagged Definition or Parameters if it
    appears below a Concept resp. a Relation.
    An empty group is always an Input for runtime data.
    
 -- Explicit Ambiguity ------------------------------------------
    
    You can have multiple group members with the
    same name but differing children:
    
    - A Concept defined with variated Definitions yields
      Variants which may be traversed through a Perspective.

    - A Relation defined with alternating Parameters
      creates Alternatives, giving you a Choice.

 ----------------------------------------------------------------}

 
type alias App
    = Zipper Word

type alias Group
    = Forest Word

    
    
type Word
    = Symbolizing Relation
    | Naming Concept

type Relation
    = More

type Concept
    = Concept String
  


  
type Tocus = Tocus (List Word)



reverse (Tocus ww) = Tocus (List.reverse ww)

neutral = Tocus []




-- Two distinct group types, depending on their parent:
type Definition = Definition Group
type Parameters = Parameters Group











-- the only way to construct a Definition is to find it:
definition : App -> Concept -> Definition
definition t concept =
    let
        asDefinition = Definition
        wantedWord = Naming concept
    in case current t of
        wantedWord -> children t |> asDefinition
        

type Leaf = Leaf


        
        
        
        




{----------------------------------------------------------------
    
    Encoding Structural Ambiguities
    
    Of the five ambiguities that an app can represent
    (see Locus.elm), two can be written in Ambilang and
    are thus explicit and immutable part of an app.
    
    Since words at a given level are unique, you can denote
    ambiguity by repeating a word several times at one level.
    
 -- Multidefinition ---------------------------------------------
    
    A filter is one of several Definitions given for a
    unique Concept. Multiple filters form a kind of table
    where each column only shows data that matches.
    
 -- Multiprototype ----------------------------------------------
    
    If there are parallel prototypes, an item may be
    Item.Ambiguous.

 -- Combining Filters and Prototypes ----------------------------

    If a Concept F, defined a both by F0 and F1, is one
    of the Prototypes of P, nothing surprising happens.

    Conversely, if F is designed such that it can append
    a prototype P0 and a prototype P1, then P0 and P1 will
    only show up in those columns that match their type, i.e.
    exactly "+ P0" resp. "+ P1", but not e.g. "P0".

 ----------------------------------------------------------------}



type Definition = D Group
type Multidefinition = DD Multiple Group
type alias DefinitionChoice = Branch Multidefinition 
    
type Prototype = P Group
type Multiprototype = PP Multiple Group
type alias PrototypeChoice = Branch Multiprotptype

type Branch context = Branch Group -- a phantom type.


type Multiple a
    = Two  a a
    | Many a (Multiple a)
    
    
    

    
-- ENCODING --
    
encodeConcept ( Concept string ) = string
encodeWord word =
    case word of
         Symbolizing More -> "+"
         Naming (Concept string) -> string
         
    
{----------------------------------------------------------------
    
    Constructing an App from a String
    
    The database stores the app in the standard UTF-8 format.

 ----------------------------------------------------------------}

https://ellie-app.com/4nBvVtFpCS6a1




























    
lines s 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

initial : App
initial =
  let
      getChildren  (Data i c) = c
      getSymbol    (Data i c) = i
  in
    Data (Name "flupsicom")
            [ Data Many
              [ Data (Name "topic")
                [ Data (Generate Title) []
                , Data Many [ Data (Generate Caption) [] ]
                , Data Many [ Data (Generate Emblem)
                                   [ Data Many [ Data (Generate Paragraph) [ Data (Generate Text) [] ] ] ] ]
                , Data Many [ Data (Generate Paragraph)
                              [ Data Many [ Data (Generate Text) [] ]
                              , Data Many [ Data (Generate Link) [] ]
                              ] ] ] ] ]
    |> Tree.build getChildren  |> Tree.map getSymbol |> Zipper.fromTree
