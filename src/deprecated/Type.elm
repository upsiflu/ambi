module Type exposing (Type, Leaf, Variation, Alternation, Definition, Parameters, encodeConcept)

import List
import Lazy.Tree as Tree exposing (Tree(..), Forest)
import Lazy.Tree.Zipper as Zipper exposing (Zipper, current, fromTree, children, root, openAll)
















{----------------------------------------------------------------
    
    Type
    
    is a representation of the immutable structure
    of an app, in the form of a lazy rose-tree.
    
    The use of the Zipper datastructure means that costly
    inferences on the type can be deferred and no initialization
    needs to be triggered. Instead, the Zipper maintains a
    construction function 'on demand'.

 -- Aspects of a Type -------------------------------------------
    
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

 
type alias Type
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
definition : Type -> Concept -> Definition
definition t concept =
    let
        asDefinition = Definition
        wantedWord = Naming concept
    in case current t of
        wantedWord -> children t |> asDefinition
        

type Leaf = Leaf


        
        
        
        




{----------------------------------------------------------------
    
    Encoding Type Ambiguities
    
    Of the five ambiguities that an app can represent
    (see Locus.elm), two can be written in Ambilang and
    are thus explicit part of the type.
    
    Since words at a given level are unique, you can denote
    ambiguity by repeating a word several times at one level.
    
 -- Variation ---------------------------------------------------
    
    A Variant is one of several Definitions given for a
    unique Concept.
    
 -- Alternation -------------------------------------------------
    
    An Alternative is one of several parameter groups given
    to a Relation.

 ----------------------------------------------------------------}


type alias Variation 
    = Multiple Variant
type Variant
    = Variant Definition
    

type alias Alternation
    = Multiple Alternative
type Alternative
    = Alternative Parameters

    
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
    
    Constructing a Type from a JSON intermediate
    
    The database stores type in the JSON format 

 ----------------------------------------------------------------}


    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


view : App -> Html msg
view a =
  section [class "App"]
    [ h2 [] [text "App Structure"]
    , let
        captionProperties l = case children l of
          (x::y::xs) ->  [class "productCaption"]
          _          ->  [class ""]
        properties l = case children l of
          (x::y::xs) ->  [class "product"]
          _          ->  [class "app"]
        viewLevel l = div (captionProperties l) [viewSymbol (current l), div (properties l) <| List.map (viewLevel) (openAll l) ]
      in
        viewLevel (root a)
    ]

viewDescription app =
  case current app of
    Template -> "(Template)"
    Many -> "(Many)"
    Name s -> s
    Generate g -> case g of
       Emblem       -> "Emblem"
       Paragraph    -> "Paragraph"
       Link         -> "Link"
       Title        -> "Title"
       Caption      -> "Caption"
       Text         -> "Text"

viewSymbol symbol =
 let
  viewContainer    = div [class "app"]
  viewType         = span [class "type"]
 in case symbol of
  Template        -> viewContainer [viewType [text " < "] ]
  Many            -> viewContainer [viewType [text " + "] ]
  Name    s       -> viewContainer [viewType [text ("'"++s++"'")] ]
  Generate g -> case g of
     Emblem       -> viewContainer [viewType [text "Emblem"] ]
     Paragraph    -> viewContainer [viewType [text "Paragraph"] ]
     Link         -> viewContainer [viewType [text "Link"] ]
     Title        -> viewContainer [viewType [text "Title"] ]
     Caption      -> viewContainer [viewType [text "Caption"] ]
     Text         -> viewContainer [viewType [text "Text"] ]

type Data = Data Symbol (List Data)

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
