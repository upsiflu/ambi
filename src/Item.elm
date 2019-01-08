module Step













type Item                                       -- persistence?

    = Form

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











interface : Item -> Interface.Entry Locus msg
interface item key =
    let
        ( primary, others ) =
            case item of


                Form

                    { concepts
                    , appending
                    Maybe <| Either               
                        App.Prototype               
                        App.Multiprototype          
                    , children: Multiple Step               -- if appendable, Edit
                    } ->
                        (
                            { primaryAction: Link Self 
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
