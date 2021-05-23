module Birds exposing (IsBird, proveIsBird)

import Animals exposing (AllKnownAnimals)
import Dict exposing (Dict)
import RefinementProofs.Knowledge exposing ( A
    , NoDomainKnowledge
    , NoNamedKnowledge
    , WithKnowledge
    , axiomaticallySetDomainKnowledge
    , forget
    , forgetNamedKnowledge
    , raw
    )
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, takeValueFromDict)


type IsBird
    = IsBird


proveIsBird :
    WithKnowledge (A (Dict Int String) animals) anyDictDomainKnowledge AllKnownAnimals anyDictNamedKnowledge
    -> WithKnowledge (A Int animalId) anyIdValueKnowledge NoDomainKnowledge (IsInDict animalId animals)
    -> Maybe (WithKnowledge (A Int animalId) anyIdValueKnowledge IsBird NoNamedKnowledge)
proveIsBird allAnimals animalId =
    let
        animalCommonName =
            takeValueFromDict animalId (forget allAnimals)
    in
    -- All birds has the word 'bird' in their names, that is just common sense
    if String.contains "bird" animalCommonName then
        Just <| axiomaticallySetDomainKnowledge IsBird <| forgetNamedKnowledge animalId

    else
        Nothing
