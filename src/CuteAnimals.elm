module CuteAnimals exposing (IsAValidCuteAnimal, proveCuteness)

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
import RefinementProofs.Proofs.NumberProofs exposing (Positive)


type IsAValidCuteAnimal
    = IsAValidCuteAnimal


proveCuteness :
    WithKnowledge (A (Dict Int String) animals) anyDictDomainKnowledge AllKnownAnimals anyDictNamedKnowledge
    -> WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals)
    -> Maybe (WithKnowledge (A Int animalId) Positive IsAValidCuteAnimal NoNamedKnowledge)
proveCuteness allAnimals animalId =
    let
        animalCommonName =
            takeValueFromDict animalId (forget allAnimals)
    in
    -- No animals with q can ever be cute, nor any animals with an id over 40, that is just common sense
    if String.contains "q" animalCommonName || raw animalId > 40 then
        Nothing

    else
        Just <| axiomaticallySetDomainKnowledge IsAValidCuteAnimal <| forgetNamedKnowledge animalId
