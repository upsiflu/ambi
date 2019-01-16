module App exposing (App, getChildSteps, initialApp, encodeWord, getString )

import List
import Lazy.Tree as Tree exposing (..)
import Lazy.Tree.Zipper as Zipper exposing (..)
















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


getFakeData : List Word -> App -> String
getFakeData = ( \steps app -> getApp steps app |> current |> encodeWord )




getApp : List Word -> App -> App
getApp tocus app = attemptOpenPath ( \word w -> word == w ) tocus app
getString tocus app = getApp tocus app |> current |> encodeWord 

getChildSteps tocus app = getApp tocus app |> Zipper.children




getChildren ( FakeApp ( w, c ) ) = c
getWord ( FakeApp ( w, c ) ) = w

type FakeApp = FakeApp ( Word, List FakeApp )
fakeApp : FakeApp
fakeApp =
    FakeApp ( Naming ( Concept "Ppp"),
        [ FakeApp ( Naming ( Concept "Beta"), [] )
        , FakeApp ( Naming ( Concept "Ceta"), [] )
        , FakeApp ( Naming ( Concept "Deta"), [] )
        , FakeApp ( Naming ( Concept "Eta"), 
            [ FakeApp ( Naming ( Concept "End of Story"), [] )
            , FakeApp ( Naming ( Concept "Other leaf"), [] )
            ] ) 
        , FakeApp ( Naming ( Concept "Feta"), [] ) 
        ] 
    )  
initialApp : App
initialApp = fakeApp |> build getChildren |> fromTree |> Zipper.map getWord
    
    
type Word
    = Symbolizing Relation
    | Naming Concept

type Relation
    = More

type Concept
    = Concept String
  
type alias Tocus = List Word

neutral : Tocus
neutral = []



        
        
        
        




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
type Multidefinition = DD ( Multiple Group )
    
type Prototype = P Group
type Multiprototype = PP ( Multiple Group )

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
         
