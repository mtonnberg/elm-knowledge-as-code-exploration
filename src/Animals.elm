module Animals exposing (AllKnownAnimals, addNewKnownAnimal, initAllKnownAnimals, lookupRealAnimal)

import Dict exposing (Dict)
import RefinementProofs.Knowledge exposing ( A
    , NoDomainKnowledge
    , NoNamedKnowledge
    , NoValueKnowledge
    , WithKnowledge
    , axiomaticallySetDomainKnowledge
    , forget
    , setNamedKnowledge
    , withName
    , withNoKnowledge
    )
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, proveKeyIsInDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)


type AllKnownAnimals
    = AllKnownAnimals


initAllKnownAnimals : WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
initAllKnownAnimals =
    axiomaticallySetDomainKnowledge AllKnownAnimals <| withNoKnowledge <| Dict.fromList [ ( 1, "Wombat" ), ( 2, "Unicorn" ), ( -1, "Imaginary horse" ) ]


addNewKnownAnimal :
    Int
    -> String
    -> WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
addNewKnownAnimal animalId animalName animals =
    axiomaticallySetDomainKnowledge AllKnownAnimals <|
        withNoKnowledge <|
            Dict.insert animalId animalName <|
                forget animals


lookupRealAnimal :
    WithKnowledge (A Int animalId) NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
    -> WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> Maybe (WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals))
lookupRealAnimal namedWantedKey namedDict =
    case ( proveKeyIsInDict (forget namedWantedKey) (forget namedDict), withName provePositive (forget namedWantedKey) ) of
        ( Just isInDictProof, Just isPositive ) ->
            let
                keyWithAllNeededProofs : WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals)
                keyWithAllNeededProofs =
                    setNamedKnowledge isInDictProof isPositive
            in
            Just keyWithAllNeededProofs

        _ ->
            Nothing
