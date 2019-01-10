module Kind













type Kind                                       -- persistence?

    = Arrangement

        { concepts: List App.Concept            -- Route
        , appending:                            --
            Maybe <| Either               
                App.Prototype               
                App.Multiprototype          
        , children: Multiple Step               -- if appendable, Edit
        }

    | Rubric

        { concepts: List App.Concept            -- Route
        , AndOr                                  
            { removable: () }                   -- yes (can't change)
            { appending:                        --
                Either
                    App.Prototype                
                    App.Multiprototype           
            }
        , children: Multiple Item               -- if appendable, Edit
        }

    | Input

        { concepts: List App.Concept            -- Route
        , removable: Bool                       -- Edit
        , preview: Edit.Data                    -- Edit
        }

    | Ambiguous

        { concepts: List App.Concept            -- Route
        , removable: ()                         -- yes (can't change)
        , possibilities: App.Multiprototype     -- 
        }


type AndOr a b
    = This a
    | That b
    | Both



editor messages =
    Interface.











































interface : Kind -> Main.Msg -> Interface.Entry Locus msg
interface item appendMsg key =
    let

        plusP prototype =
            { primaryAction: |> Interface.Button
            , contextAction: Nothing
            , view: div [ class "plus" ] [ text "+" ]
            }

        ( primary, others ) =
            case item of


                Form

                    { concepts
                    , appending          
                    , children
                    } -> (  { primaryAction: Link Self 
                            , contextAction: Nothing
                            , view: text "=== FORM ==="
                            }
                         , 
                            case appending of
                                Nothing -> []
                                Just proto ->
                                    case proto of
                                        Left single ->
                                            [ plusP single ]
                                        Right multi ->
                                            [ plusPP multi ]
                         )

        Rubric

        { concepts: List App.Concept            -- Route
        , AndOr                                  
            { removable: () }                   -- yes (can't change)
            { appending:                        --
                Either
                    App.Prototype                
                    App.Multiprototype           
            }
        , children: Multiple Item               -- if appendable, Edit
        }

        Input

        { concepts: List App.Concept            -- Route
        , removable: Bool                       -- Edit
        , preview: Edit.Data                    -- Edit
        }

        Ambiguous

        { concepts: List App.Concept            -- Route
        , removable: ()                         -- yes (can't change)
        , possibilities: App.Multiprototype     -- 
        }

    in
        { signature: key
        , contextMenu: Nothing
        , tabstops: Interface.ring primary others
        }
