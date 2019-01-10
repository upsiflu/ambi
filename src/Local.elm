module Local

import Maybe.Extra as Maybe









type alias Cache = Dict Locus SyncRequest




syncStateIndicator : Cache -> (Locus -> String)

syncStateIndicator =
    get >> (\syncRequest -> Maybe.unpack (always "") identity ) 
    
