module App exposing (App, getFirstSteps, initialApp, wordToStep, getString, encodeConcept )

import Lazy.Tree as Tree exposing ( build, Forest )
import Lazy.Tree.Zipper as Zipper exposing ( Zipper, openAll, current, fromTree, attemptOpenPath )
import List exposing ( reverse, map )

import Stack exposing ( .. )














{-
    
    App
    
    is an immutable structure, written in ambilang,
    known to the computer in the form of a lazy rose-tree.
    
    The use of the Zipper datastructure means that costly
    inferences on the app can be deferred and no initialization
    needs to be triggered. Instead, the Zipper maintains a
    construction function 'on demand'.

 -- Constellations of an App ------------------------------------
    
    Indentations denote parent-child relation,
    Symbols +, <, >, : denote mappings over children;
    any other word denotes a concept, whereby nested concepts
    are compressed to one node.
    
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

 -}

 
type alias App
    = Zipper Word

type alias Group
    = Forest Word
 
type alias Step
    = String

type alias Locus
    = Stack Step

getFirstSteps : Locus -> App -> List Step
getFirstSteps l a =
    getApp l a |> openAll |> map current |> map wordToStep 
        
getString : Locus -> App -> String
getString l a =
    getApp l a |> current |> wordToStep 

getApp : Locus -> App -> App
getApp l app =
    attemptOpenPath ( \step word -> wordToStep word == step ) ( reverse l ) app
 


------------------------------------------------------

getFakeData : Locus -> App -> String
getFakeData = ( \steps app -> getApp steps app |> current |> wordToStep )

type FakeApp = FakeApp ( Word, List FakeApp )
fakeApp : FakeApp
fakeApp =
    FakeApp ( Naming ( Concept "Blog"),
        [ FakeApp ( Naming ( Concept "Title"), [] )
        , FakeApp ( Naming ( Concept "Introduction Paragraph"), [] )
        , FakeApp ( Naming ( Concept "Contact"), [] )
        , FakeApp ( Naming ( Concept "Posts"), 
            [ FakeApp ( Naming ( Concept "Post 1"), [] )
            , FakeApp ( Naming ( Concept "Post 2"), [] )
            ] ) 
        , FakeApp ( Naming ( Concept "Footer Paragraph"), [] ) 
        ] 
    )

initialApp : App
initialApp = fakeApp |> build getChildren |> fromTree |> Zipper.map getWord
getChildren ( FakeApp ( w, c ) ) = c |> reverse
getWord ( FakeApp ( w, c ) ) = w
    
--------------------------------------------------------------
    
    
    
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

wordToStep word =
    case word of
         Symbolizing More -> "+"
         Naming (Concept string) -> string
         
 
